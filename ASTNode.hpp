#pragma once

#include <cmath>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include "lexer.hpp"
#include "SymbolTable.hpp"

// The base class for Abstract Syntax Tree nodes.
class ASTNode {
protected:
  size_t line_num;         // What line was this node parsed from in the original file?

public:
  using ptr_t = std::unique_ptr<ASTNode>;

  ASTNode() : line_num(0) { }
  ASTNode(size_t line_num) : line_num(line_num) { }
  ASTNode(const ASTNode &) = default;
  ASTNode(ASTNode &&) = default;
  virtual ~ASTNode() { }
  ASTNode & operator=(const ASTNode &) = default;
  ASTNode & operator=(ASTNode &&) = default;

  virtual std::string GetTypeName() const = 0;
  virtual double Run(SymbolTable & /* symbols */) = 0;
  virtual void Print(std::string prefix="") const {
    std::cout << prefix << GetTypeName() << std::endl;
  }

  virtual bool CanAssign() const { return false; }
  virtual double Assign(double value, SymbolTable & /* symbols */) const {
    assert(false);  // Do not call Assign() on a node that can't run it.
    return value;
  }
};

// Nodes that have children; can be used as a base class OR for a series of statements.
class ASTNode_Block : public ASTNode {
private:
  std::vector< ptr_t > children{};

public:
  template <typename... NODE_Ts>
  ASTNode_Block(size_t line_num, NODE_Ts &&... nodes) : ASTNode(line_num) {
    (AddChild(std::move(nodes)), ...);
  }

  std::string GetTypeName() const override { return "BLOCK"; }

  // Tools to work with child nodes...
  size_t NumChildren() const { return children.size(); }
  bool HasChild(size_t id) const { return id < children.size() && children[id]; }

  template <typename NODE_T, typename... ARG_Ts>
  void MakeChild(ARG_Ts &&... args) {
    children.push_back( std::make_unique<NODE_T>(std::forward<ARG_Ts>(args)...) );
  }

  void AddChild(ptr_t && child) {
    children.push_back(std::move(child));
  }

  bool CanAssignChild(size_t id) const {
    assert(HasChild(id));
    return children[id]->CanAssign();
  }
  double AssignChild(size_t id, double value, SymbolTable & symbols) {
    assert(HasChild(id));
    return children[id]->Assign(value, symbols);
  }

  double RunChild(size_t id, SymbolTable & symbols) {
    assert(HasChild(id));
    return children[id]->Run(symbols);
  }

  double Run(SymbolTable & symbols) override {
    // Loop through all children, running them in order.
    for (auto & child : children) child->Run(symbols);
    return 0.0;
  }

  void Print(std::string prefix="") const override {
    // Print self and then all children, in order.
    std::cout << prefix << GetTypeName() << std::endl;
    for (auto & child : children) child->Print(prefix);
  }
};

class ASTNode_If : public ASTNode_Block {
public:
  // Constructors can take either just a "then" child or both "then" and "else"
  ASTNode_If(size_t line_num, ptr_t && test, ptr_t && action)
    : ASTNode_Block(line_num, test, action) { }
  ASTNode_If(size_t line_num, ptr_t && test, ptr_t && action, ptr_t && alt_action)
    : ASTNode_Block(line_num, test, action, alt_action) { }

  std::string GetTypeName() const override { return "IF"; }

  double Run(SymbolTable & symbols) override {
    assert(NumChildren() >= 2 && NumChildren() <= 3);
    double result = RunChild(0,symbols);
    if (result != 0.0) RunChild(1,symbols);
    else if (NumChildren() == 3) RunChild(2,symbols);
    return 0.0;
  }
};

class ASTNode_While : public ASTNode_Block {
public:
  ASTNode_While(size_t line_num, ptr_t && test, ptr_t && action)
    : ASTNode_Block(line_num, test, action) { }

  std::string GetTypeName() const override { return "WHILE"; }

  double Run(SymbolTable & symbols) override {
    assert(NumChildren() == 2);
    while (RunChild(0, symbols) != 0.0) {
      if (HasChild(1)) RunChild(1, symbols);
    }
    return 0.0;
  }
};

// An ASTNode to handle unary math operations, currently NOT ('!') and NEGATE ('-')
class ASTNode_Math1 : public ASTNode_Block {
protected:
  std::string op;
public:
  ASTNode_Math1(size_t line_num, std::string op, ptr_t && child)
    : ASTNode_Block(line_num, child), op(op) { }
  ASTNode_Math1(const emplex::Token & token, ptr_t && child)
    : ASTNode_Math1(token.line_id, token.lexeme, std::move(child)) { }

  std::string GetTypeName() const override { return "MATH1"; }

  double Run(SymbolTable & symbols) override {
    assert(NumChildren() == 1);

    const double out = RunChild(0, symbols);
    if (op == "!") return (out == 0.0) ? 1.0 : 0.0;
    else if (op == "-") return -out;

    Error(line_num, "Unknown unary operator '", op, "'.");
    exit(2);

    return 0.0;
  }
};

// An ASTNode to handle all binary math operations
class ASTNode_Math2 : public ASTNode_Block {
protected:
  std::string op;
public:
  ASTNode_Math2(size_t line_num, std::string op, ptr_t && child1, ptr_t && child2)
    : ASTNode_Block(line_num, child1, child2), op(op) { }
  ASTNode_Math2(const emplex::Token & token, ptr_t && child1, ptr_t && child2)
    : ASTNode_Block(token.line_id, std::move(child1), std::move(child2)), op(token.lexeme) { }

  std::string GetTypeName() const override { return "MATH2"; }

  double Run(SymbolTable & symbols) override {
    assert(NumChildren() == 2);

    // If we are doing an assignment, we need to handle it specially.
    if (op == "=") {
      if (!CanAssignChild(0)) {
        Error(line_num, "Left-hand-side of assignment must be a variable.");
      }
      return AssignChild(0, RunChild(1, symbols), symbols);
    }

    // Always get result from the first child.
    const double arg1 = RunChild(0, symbols);

    // Deal with short circuiting if needed.
    if (op == "||") { if (arg1 != 0.0) return 1.0; }
    else if (op == "&&") { if (arg1 == 0.0) return 0.0; }

    // If we didn't short-circuit, get result from second child.
    const double arg2 = RunChild(1, symbols);

    // Perform the correct operation.
    if (op == "**") return std::pow(arg1, arg2);
    if (op == "*") return arg1 * arg2;
    if (op == "/") {
      if (arg2 == 0.0) Error(line_num, "Division by zero.");
      return arg1 / arg2;
    }
    if (op == "%") {
      if (arg2 == 0.0) Error(line_num, "Modulus by zero.");
      return std::fmod(arg1, arg2);
    }
    if (op == "+") return arg1 + arg2;
    if (op == "-") return arg1 - arg2;

    if (op == "<") return arg1 < arg2;
    if (op == "<=") return arg1 <= arg2;
    if (op == ">") return arg1 > arg2;
    if (op == ">=") return arg1 >= arg2;
    if (op == "==") return arg1 == arg2;
    if (op == "!=") return arg1 != arg2;
    
    if (op == "||" || op == "&&") return (arg2 == 0.0) ? 0.0 : 1.0;

    Error(line_num, "Unknown binary operator '", op, "'.");
    return 0.0;
  }
};

// Leaf node to handle a literal number
class ASTNode_NumLit : public ASTNode {
protected:
  double value = 0.0;

public:
  ASTNode_NumLit(size_t line_num, double value)
    : ASTNode(line_num), value(value) { }

  std::string GetTypeName() const override { return "NUM_LIT"; }

  double Run(SymbolTable & /* symbols */) override {
    return value;
  }
};

// Leaf node to handle a variable
class ASTNode_Var : public ASTNode {
protected:
  size_t var_id;

public:
  ASTNode_Var(size_t line_num, size_t id) : ASTNode(line_num), var_id(id) { }
  ASTNode_Var(const emplex::Token & token, SymbolTable & symbols)
    : ASTNode(token.line_id), var_id(symbols.GetVarID(token.lexeme)) { }

  std::string GetTypeName() const override { return "VAR"; }

  bool CanAssign() const override { return true; }
  double Assign(double value, SymbolTable & symbols) const override {
    return symbols.SetValue(var_id, value);
  }

  double Run(SymbolTable & symbols) override {
    return symbols.GetValue(var_id);
  }
};

class ASTNode_Print : public ASTNode_Block {
protected:
  std::vector<std::string> strings{};
public:
  ASTNode_Print(size_t line_num, ptr_t && child)
    : ASTNode_Block(line_num, child), strings{"",""}
  { }

  ASTNode_Print(size_t line_num, std::string print_info, SymbolTable & symbols)
    : ASTNode_Block(line_num)
  {
    // Remove quotes at beginning and end.
    print_info = print_info.substr(1, print_info.size() - 2);

    // Fix all occurrences of \n, \" and \\ with the correct ASCII char.
    ReplaceAll(print_info, "\\n", "\n");
    ReplaceAll(print_info, "\\\"", "\"");
    ReplaceAll(print_info, "\\\\", "\\");

    // Break the control sequence into sub-strings and variables.
    size_t start_pos = 0;
    for (size_t brace_pos = print_info.find('{');
          brace_pos != std::string::npos;
          brace_pos = print_info.find('{', brace_pos+1))
    {
      // If there was a backslash before this brace, remove the backslash and skip the brace.
      if (brace_pos > 0 && print_info[brace_pos-1] == '\\') {
        print_info.erase(--brace_pos);
        continue;
      }

      // Find the end of this brace.
      size_t end_pos = print_info.find('}', brace_pos+1);
      if (end_pos == std::string::npos) {
        Error(line_num, "No matching close brace for open brace in print.");
      }

      // Save the string from before this brace.
      strings.push_back( print_info.substr(start_pos, brace_pos-start_pos) );
      start_pos = end_pos+1;  // Move the start pos to after the close brace.

      // Save the variable inside the braces.
      size_t var_len = end_pos - brace_pos - 1;
      std::string var_name = print_info.substr(brace_pos+1, var_len);
      size_t var_id = symbols.GetVarID(var_name);
      if (var_id == SymbolTable::NO_ID) {
        Error(line_num, "Trying to print unknown variable '", var_name, "'.");
      }

      MakeChild<ASTNode_Var>(line_num, var_id);
    }

    // Save the string from the last close brace to the end.
    strings.push_back( print_info.substr(start_pos, print_info.size()-start_pos) );

  }

  std::string GetTypeName() const override { return "PRINT"; }

  double Run(SymbolTable & symbols) override {
    for (size_t i = 0; i < NumChildren(); ++i) {
      std::cout << strings[i];
      std::cout << RunChild(i, symbols);
    }
    std::cout << strings.back() << std::endl;
    return 0.0;
  }
};

