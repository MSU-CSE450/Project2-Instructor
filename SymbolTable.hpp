#pragma once

#include <assert.h>
#include <string>
#include <unordered_map>
#include <vector>

#include "tools.hpp"


class SymbolTable {
private:
  // Information to track about every variable in the table.
  struct VarInfo {
    std::string name;
    size_t def_line;
    double value;
  };

  // Store all of the informaiton for each variable by id (i.e., index into vector)
  std::vector< VarInfo > var_array{};

  // Each scope maps variable names to that variable's id (position) in var_array
  using scope_t = std::unordered_map<std::string, size_t>;

  // Keep a stack of active scopes as we process the file (scope 0 is global)
  std::vector< scope_t > scope_stack{1};

public:
  static constexpr size_t NO_ID = static_cast<size_t>(-1);

  void PushScope() { scope_stack.emplace_back(); }
  void PopScope() {
    assert(scope_stack.size() > 1); // First level is global -- do not delete!
    scope_stack.pop_back();
  }

  // Test if a given variable ID exists in the symbol table.
  bool Has(size_t id) const { return id < var_array.size(); }

  // Test if a given identifier exists anywhere in the symbol table.
  bool Has(std::string name) {
    for (const scope_t & scope : scope_stack) {
      if (scope.count(name)) return true;
    }
    return false;
  }

  // Add a new variable with the provided identifier.
  size_t AddVar(std::string name, size_t line_num) {
    // New variables go in the last (outermost) scope.
    scope_t & table = scope_stack.back();

    // If a variable by this name already exists in this scope, throw an error.
    if (table.count(name)) {
      const auto & id = table[name];
      Error(line_num, "Redeclaration of variable '", name,
            "' (original declaration on line ", var_array[id].def_line, ").");
    }

    // Add the new variable to this scop and return this id.
    const size_t id = var_array.size();
    var_array.emplace_back(VarInfo{name, line_num, 0.0});
    table[name] = id;
    return id;
  }

  // Scan through symbol table, from top down, to find the correct variable and
  // return its unique ID.
  size_t GetVarID(std::string name) const {
    for (auto scope_it = scope_stack.rbegin(); scope_it < scope_stack.rend(); ++scope_it) {
      // Look for the variable in this scope; if we find it, print its ID.
      auto var_it = scope_it->find(name);
      if (var_it != scope_it->end()) return var_it->second;
    }
    return NO_ID; // Not found in any scope!
  }

  // Return the current value of the variable with the provided ID.
  double GetValue(size_t id) const {
    assert(id < var_array.size());
    return var_array[id].value;
  }

  // Set (and return) the variable with the specified ID to the provided value.
  double SetValue(size_t id, double value) {
    return var_array[id].value = value;
  }

  // For debugging, print all of the variables currently in the symbol table,
  // along with the line they were defined on and their current values.
  void Print() const {
    std::cout << var_array.size() << " variables found:" << std::endl;
    for (const VarInfo & v : var_array) {
      std::cout << " '" << v.name << "' (line: "
                << v.def_line << ") = " << v.value << std::endl;
    }
  }
};
