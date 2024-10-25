#pragma once

#include <cmath>
#include <sstream>
#include <string>
#include <vector>

#include "lexer.hpp"
#include "SymbolTable.hpp"

class ASTNode {
public:
  // Allowed types for an ASTNode
  enum Type {
    EMPTY=0,  // No operation; placeholder that should not be put in AST.

    // Internal node types:
    BLOCK,    // A series of independent statements.
    STRING,   // A string to be used in printing.
    IF,
    WHILE,
    PRINT,
    MATH1,    // Unary math operation; `info` specifies which operation.
    MATH2,    // Binary math operation; `info` specifies which operation.

    // Leaf node types:
    NUM_LIT,     // A number, such as 123.45 ; value stored in "value".
    VAR,         // A variable; ID stored in "value".
  };

private:
  size_t line_num;         // What line was this node parsed from in the original file?
  Type type = EMPTY;
  std::string info{};      // Content info, such as math operation to perform.
  double value = -1.0;     // Value info, such as id of variable or value of a literal.
  std::vector<ASTNode> children{};

public:
  ASTNode() : line_num(0), type(EMPTY) { }
  ASTNode(size_t line_num, Type type) : line_num(line_num), type(type) { }
  ASTNode(const ASTNode &) = default;
  ASTNode(ASTNode &&) = default;
  ASTNode & operator=(const ASTNode &) = default;
  ASTNode & operator=(ASTNode &&) = default;

  Type GetType() const { return type; }
  std::string GetTypeName() const {
    switch (type) {
    case EMPTY:   return "EMPTY";
    case BLOCK:   return "BLOCK";
    case IF:      return "IF";
    case WHILE:   return "WHILE";
    case PRINT:   return "PRINT";
    case MATH1:   return "MATH1";
    case MATH2:   return "MATH2";
    case NUM_LIT: return "NUM_LIT";
    case VAR:     return "VAR";
    default:      return "UNKNOWN";
    }
    return "UNKNOWN";
  }

  std::string GetInfo() const { return info; }
  void SetInfo(std::string in_info) { info = in_info; }

  double GetValue() const { return value; }
  size_t GetIDValue() const { return static_cast<size_t>(value); }
  void SetValue(double in_val) { value = in_val; }

  const ASTNode & GetChild(size_t id) const { return children[id]; }
  const std::vector<ASTNode> & GetChildren() const { return children; }
  void AddChild(ASTNode && node) { children.emplace_back(node); }

  bool IsEmpty() const { return !type; }

  std::string DebugString() const {
    std::stringstream ss;
    ss << "Node type=\"" << GetTypeName() << "\""
       << "  info=\"" << info << "\""
       << "  value=" << value
       << "  children=" << children.size();
    return ss.str();
  }

  double Run(SymbolTable & symbols) {
    switch (type) {
    case EMPTY:    break;  // And throw an error since this shouldn't be in the tree?
    case BLOCK:    Run_BLOCK(symbols); break;
    case IF:       Run_IF(symbols);    break;
    case WHILE:    Run_WHILE(symbols); break;
    case PRINT:    Run_PRINT(symbols); break;

    case MATH1:    return Run_MATH1(symbols);
    case MATH2:    return Run_MATH2(symbols);
    case NUM_LIT:  return Run_NUM_LIT(symbols);
    case VAR:      return Run_VAR(symbols);
    default:       return 0.0;
    }

    return 0.0;
  }

  void Run_BLOCK(SymbolTable & symbols) {
    for (auto & child : children) child.Run(symbols);
  }

  void Run_IF(SymbolTable & symbols) {
    assert(children.size() >= 2 && children.size() <= 3);
    double result = children[0].Run(symbols);
    if (result != 0.0) children[1].Run(symbols);
    else if (children.size() == 3) children[2].Run(symbols);
  }

  void Run_WHILE(SymbolTable & symbols) {
    assert(children.size() == 2);
    while (children[0].Run(symbols) != 0.0) {
      children[1].Run(symbols);
    }
  }

  void Run_PRINT(SymbolTable & symbols) {
    // Step through children.  Print strings directly and process expressions.
    for (auto & child : children) {
      if (child.type == STRING) std::cout << child.info;
      else std::cout << child.Run(symbols);
    }
    std::cout << std::endl;
  }

  double Run_MATH1(SymbolTable & symbols) {
    assert(children.size() == 1);

    const double out = children[0].Run(symbols);
    if (info == "!") return (out == 0.0) ? 1.0 : 0.0;
    else if (info == "-") return -out;

    Error(line_num, "Unknown unary operator '", info, "'.");
    exit(2);

    return 0.0;
  }

  double Run_MATH2(SymbolTable & symbols) {
    assert(children.size() == 2);

    // If we are doing an assignment, we need to handle it specially.
    if (info == "=") {
      if (children[0].GetType() != VAR) {
        Error(line_num, "Left-hand-side of assignment must be a variable.");
      }
      size_t var_id = children[0].GetIDValue();
      double value = children[1].Run(symbols);
      symbols.SetValue(var_id, value);
      return value;
    }

    const double arg1 = children[0].Run(symbols);

    // Deal with short circuiting.
    if (info == "||") { if (arg1 != 0.0) return 1.0; }
    else if (info == "&&") { if (arg1 == 0.0) return 0.0; }

    const double arg2 = children[1].Run(symbols);

    if (info == "**") return std::pow(arg1, arg2);
    if (info == "*") return arg1 * arg2;
    if (info == "/") {
      if (arg2 == 0.0) Error(line_num, "Division by zero.");
      return arg1 / arg2;
    }
    if (info == "%") {
      if (arg2 == 0.0) Error(line_num, "Modulus by zero.");
      return std::fmod(arg1, arg2);
    }
    if (info == "+") return arg1 + arg2;
    if (info == "-") return arg1 - arg2;

    if (info == "<") return arg1 < arg2;
    if (info == "<=") return arg1 <= arg2;
    if (info == ">") return arg1 > arg2;
    if (info == ">=") return arg1 >= arg2;
    if (info == "==") return arg1 == arg2;
    if (info == "!=") return arg1 != arg2;
    
    if (info == "||" || info == "&&") return (arg2 == 0.0) ? 0.0 : 1.0;

    Error(line_num, "Unknown binary operator '", info, "'.");
    exit(2);

    return 0.0;
  }

  double Run_NUM_LIT(SymbolTable & /*symbols*/) {
    return value;
  }

  double Run_VAR(SymbolTable & symbols) {
    const size_t var_id = GetIDValue();
    return symbols.GetValue(var_id);
  }

};


ASTNode ASTVarNode(size_t line_num, size_t id) {
  ASTNode node{line_num, ASTNode::VAR};
  node.SetValue(static_cast<double>(id));
  return node;
}

// Turn a specified identifier into a node.
// (Currently can only be a var, but other IDs allowed in the future.)
ASTNode ASTVarNode(const emplex::Token & token, const SymbolTable & symbols) {
  size_t var_id = symbols.GetVarID(token.lexeme);
  if (var_id == SymbolTable::NO_ID) {
    Error(token.line_id, "Unknown identifier '", token.lexeme, "'.");
  }
  return ASTVarNode(token.line_id, var_id);
}

ASTNode ASTLiteralNode(size_t line_num, double value) {
  ASTNode node{line_num, ASTNode::NUM_LIT};
  node.SetValue(value);
  return node;
}

ASTNode ASTMathNode(size_t line_num, std::string op, ASTNode && child) {
  ASTNode node(line_num, ASTNode::MATH1);
  node.SetInfo(op);
  node.AddChild(std::move(child));
  return node;
}

ASTNode ASTMathNode(size_t line_num, std::string op, ASTNode && child1, ASTNode && child2) {
  ASTNode node(line_num, ASTNode::MATH2);
  node.SetInfo(op);
  node.AddChild(std::move(child1));
  node.AddChild(std::move(child2));
  return node;
}

template <typename... Ts>
ASTNode ASTMathNode(emplex::Token token, Ts &&... args) {
  return ASTMathNode(token.line_id, token.lexeme, std::forward<Ts>(args)...);
}

ASTNode ASTStringNode(size_t line_num, std::string str) {
  ASTNode node(line_num, ASTNode::STRING);
  node.SetInfo(str);
  return node;
}