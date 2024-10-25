#pragma once

#include <assert.h>
#include <string>
#include <unordered_map>
#include <vector>

#include "tools.hpp"


class SymbolTable {
private:
  struct VarInfo {
    std::string name;
    size_t def_line;
    double value;
  };

  // Track all of the individual variables values.
  std::vector< VarInfo > var_array{};

  // Track variable names in a scope to ids (positions) in var_array
  using scope_t = std::unordered_map<std::string, size_t>;

  // Keep a stack of active scopes as we process the file (start at global)
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

  // Add a variable with the provided identifier.
  size_t AddVar(std::string name, size_t line_num) {
    scope_t & table = scope_stack.back();
    if (table.count(name)) {
      const auto & id = table[name];
      Error(line_num, "Redeclaration of variable '", name,
            "' (original declaration on line ", var_array[id].def_line, ").");
    }
    const size_t id = var_array.size();
    var_array.emplace_back(VarInfo{name, line_num, 0.0});
    table[name] = id;
    return id;
  }

  // Scan through symbol table, in order, to find the correct variable.
  size_t GetVarID(std::string name) const {
    for (auto scope_it = scope_stack.rbegin(); scope_it < scope_stack.rend(); ++scope_it) {
      // Look for the variable in this scope; if we find it, print its ID.
      auto var_it = scope_it->find(name);
      if (var_it != scope_it->end()) return var_it->second;
    }
    return NO_ID; // Not found in any scope!
  }

  double GetValue(size_t id) const {
    assert(id < var_array.size());
    return var_array[id].value;
  }

  void SetValue(size_t id, double value) {
    var_array[id].value = value;
  }

  void Print() const {
    std::cout << var_array.size() << " variables found:" << std::endl;
    for (const VarInfo & v : var_array) {
      std::cout << " '" << v.name << "' (line: "
                << v.def_line << ") = " << v.value << std::endl;
    }
  }
};
