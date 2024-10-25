#include <assert.h>
#include <fstream>
#include <string>
#include <unordered_map>
#include <vector>

#include "ASTNode.hpp"
#include "lexer.hpp"
#include "SymbolTable.hpp"
#include "TokenQueue.hpp"

class MACROCalc {
private:
  TokenQueue tokens;

  SymbolTable symbols{};
  ASTNode root{0, ASTNode::BLOCK};

  struct OpInfo {
    size_t level;
    char assoc;   // l=left; r=right; n=non
  };
  std::unordered_map<std::string, OpInfo> op_map{};

  // == HELPER FUNCTIONS

  template <typename... Ts>
  void TriggerError(Ts... message) {
    if (tokens.None()) tokens.Rewind();
    Error(tokens.CurLine(), std::forward<Ts>(message)...);
  }

  void SetupOperators() {
    // Setup operator precedence.
    size_t cur_prec = 0;
    op_map["("]  = op_map["!"]  =               OpInfo{cur_prec++, 'n'};
    op_map["**"] =                              OpInfo{cur_prec++, 'r'};
    op_map["*"]  = op_map["/"]  = op_map["%"] = OpInfo{cur_prec++, 'l'};
    op_map["+"]  = op_map["-"]  =               OpInfo{cur_prec++, 'l'};
    op_map["<"]  = op_map["<="] = op_map[">"] = op_map[">="] = OpInfo{cur_prec++, 'n'};
    op_map["=="] = op_map["!="] =               OpInfo{cur_prec++, 'n'};
    op_map["&&"] =                              OpInfo{cur_prec++, 'l'};
    op_map["||"] =                              OpInfo{cur_prec++, 'l'};
    op_map["="]  =                              OpInfo{cur_prec++, 'r'};
  }

public:
  MACROCalc(std::string filename) {    
    std::ifstream in_file(filename);              // Load the input file
    if (in_file.fail()) {
      std::cout << "ERROR: Unable to open file '" << filename << "'." << std::endl;
    }

    tokens.Load(in_file);  // Load all tokens from the file.

    SetupOperators();
  }

  // Convert any token representing a unary value into an ASTNode.
  // (i.e., a leaf in an expression and associated unary operators)
  ASTNode Parse_UnaryTerm() {
    const emplex::Token & token = tokens.Use();
    ASTNode out{};

    // Check operator type
    switch (token.id) {
    case '+':   // Skip any operation.
      out = Parse_UnaryTerm();
      break;
    case '-':   // Attach unary operators to the term that follows.
    case '!':
      out = ASTMathNode(token, Parse_UnaryTerm());
      break;
    case '(':   // Allow full expressions in parentheses.
      out = Parse_Expression();
      tokens.Use(')');
      break;
    case emplex::Lexer::ID_ID:
      out = ASTVarNode(token, symbols);
      break;
    case emplex::Lexer::ID_LIT_VALUE:
      out = ASTLiteralNode(token.line_id, std::stod(token.lexeme));
      break;
    default:
      TriggerError("Unexpected token '", token.lexeme, "'");
    }

    return out;
  }

  // Parse expressions.  The level input determines how restrictive this parse should be.
  // Only continue processing with types at the target level or higher.
  ASTNode Parse_Expression(size_t prec_limit=1000) {
    // Any expression must begin with a variable name or a literal value.
    ASTNode cur_node = Parse_UnaryTerm();

    size_t skip_prec = 1000; // If we get a non-associative op, we must skip next one.

    // While there are more tokens to process, try to expand this expression.
    while (tokens.Any()) {
      // Peek at the next token; if it is an op, keep going and get its info.
      auto op_token = tokens.Peek();
      if (!op_map.count(op_token.lexeme)) break;  // Not an op token; stop here!
      OpInfo op_info = op_map[op_token.lexeme];

      // If precedence of next operator is too high, return what we have.
      if (op_info.level > prec_limit) break;

      // If the next precedence is not allowed, throw an error.
      if (op_info.level == skip_prec) {
        TriggerError("Operator '", op_token.lexeme, "' is non-associative.");
      }

      // If we made it here, we have a binary operation to use, so consume it.
      tokens.Use();

      // Find the allowed precedence for the next term.
      size_t next_limit = op_info.level;
      if (op_info.assoc != 'r') --next_limit;

      // Load the next term.
      ASTNode node2 = Parse_Expression(next_limit);

      cur_node = ASTMathNode(op_token, std::move(cur_node), std::move(node2));

      skip_prec = (op_info.assoc == 'n') ? op_info.level : 1000;
    }

    return cur_node;
  }

  ASTNode Parse_Statement() {
    // Test what kind of statement this is and call the appropriate function...
    switch (tokens.Peek()) {
      using namespace emplex;
      case Lexer::ID_VAR:   return Parse_Statement_Var();
      case Lexer::ID_IF:    return Parse_Statement_If();
      case Lexer::ID_WHILE: return Parse_Statement_While();
      case Lexer::ID_PRINT: return Parse_Statement_Print();
      case '{': return Parse_StatementList();
      case ';':
        tokens.Use();
        return ASTNode{};
      default: return Parse_Statement_Expression();
    }
  }

  ASTNode Parse_Statement_Var() {
     // Variable declarations shouldn't be called without 'var' keyword.
    tokens.Use(emplex::Lexer::ID_VAR);
    const auto var_token =
      tokens.Use(emplex::Lexer::ID_ID, "Keyword 'var' must be followed by identifier.");
    symbols.AddVar(var_token.lexeme, var_token.line_id);
    if (tokens.UseIf(';')) {
      return ASTNode(var_token.line_id, ASTNode::EMPTY);  // Variable added, nothing else to do.
    }
    tokens.Use('=', "Expected ';' or '=' after declaration of variable '", var_token.lexeme, "'.");
    auto expr_node = Parse_Expression();
    tokens.Use(';');
    return ASTMathNode(var_token.line_id, "=", ASTVarNode(var_token,symbols), std::move(expr_node));
  }

  ASTNode Parse_Statement_If() {
    auto if_token = tokens.Use(emplex::Lexer::ID_IF);
    tokens.Use('(', "If commands must be followed by a '(");
    ASTNode condition = Parse_Expression();
    tokens.Use(')');
    ASTNode body = Parse_Statement();
    ASTNode out(if_token.line_id, ASTNode::IF);
    out.AddChild(std::move(condition));
    out.AddChild(std::move(body));

    // Check if we need to add on an "else" branch
    if (tokens.UseIf(emplex::Lexer::ID_ELSE)) {
      ASTNode alt = Parse_Statement();
      out.AddChild(std::move(alt));
    }

    return out;
  }

  ASTNode Parse_Statement_While() {
    auto while_token = tokens.Use(emplex::Lexer::ID_WHILE);
    tokens.Use('(', "While commands must be followed by a '(");
    ASTNode condition = Parse_Expression();
    tokens.Use(')');
    ASTNode body = Parse_Statement();
    ASTNode out{while_token.line_id, ASTNode::WHILE};
    out.AddChild(std::move(condition));
    out.AddChild(std::move(body));
    return out;
  }

  ASTNode Parse_Statement_Print() {
    auto print_token = tokens.Use(emplex::Lexer::ID_PRINT);
    size_t line_num = print_token.line_id;
    tokens.Use('(', "Print commands must be followed by a '(");
    ASTNode out{line_num, ASTNode::PRINT};
    if (tokens.Any() && tokens.Peek().id == emplex::Lexer::ID_LIT_STRING) {
      std::string print_info = tokens.Use().lexeme;

      // Remove quotes at beginning and end.
      print_info = print_info.substr(1, print_info.size() - 2);
      // Fix all occurrences of \n, \" and \\ with the correct ASCII char.
      ReplaceAll(print_info, "\\n", "\n");
      ReplaceAll(print_info, "\\\"", "\"");
      ReplaceAll(print_info, "\\\\", "\\");

      // Break the string into sub-strings and variables.
      size_t start_pos = 0;
      for (size_t brace_pos = print_info.find('{');
           brace_pos != std::string::npos;
           brace_pos = print_info.find('{', brace_pos+1))
      {
        // If there was a backslash before this brace, skip it.
        if (brace_pos > 0 && print_info[brace_pos-1] == '\\') {
          print_info.erase(--brace_pos);
          continue;
        }

        // Find the end of this brace.
        size_t end_pos = print_info.find('}', brace_pos+1);
        if (end_pos == std::string::npos) {
          std::cout << "ERROR: No matching close brace for open brace in print." << std::endl;
          exit(1);
        }

        // Save the string from before this brace, if any.
        if (brace_pos != start_pos) {
          out.AddChild(
            ASTStringNode(line_num, print_info.substr(start_pos, brace_pos-start_pos))
          );
        }
        start_pos = end_pos+1;  // Move the start pos to after the close brace.

        // Save the variable inside the braces.
        size_t var_len = end_pos - brace_pos - 1;
        std::string var_name = print_info.substr(brace_pos+1, var_len);
        size_t var_id = symbols.GetVarID(var_name);
        if (var_id == SymbolTable::NO_ID) {
          std::cout << "ERROR: Trying to print unknown variable '"
                    << var_name << "'." << std::endl;
          exit(1);
        }
        out.AddChild(ASTVarNode(line_num, var_id));
      }

      // Save the string from the last close brace to the end.
      if (start_pos < print_info.size()) {
        out.AddChild(
          ASTStringNode(line_num, print_info.substr(start_pos, print_info.size()-start_pos))
        );
      }

    }
    else {
      out.AddChild(Parse_Expression());
    }

    while (tokens.UseIf(',')) {
      out.AddChild(Parse_Expression());
    }
    tokens.Use(')');
    tokens.Use(';');
    return out;
  }

  ASTNode Parse_Statement_Expression() {
    ASTNode out = Parse_Expression();
    tokens.Use(';');
    return out;
  }

  ASTNode Parse_StatementList() {
    ASTNode out_node(tokens.Peek().line_id, ASTNode::BLOCK);
    tokens.Use('{', "Statement blocks must start with '{'.");
    symbols.PushScope();
    while (tokens.Any() && tokens.Peek() != '}') {
      ASTNode statement = Parse_Statement();
      if (!statement.IsEmpty()) out_node.AddChild( std::move(statement) );
    }
    symbols.PopScope();
    tokens.Use('}', "Statement blocks must end with '}'.");
    return out_node;
  }

  void Parse() {
    while (tokens.Any()) {
      ASTNode node = Parse_Statement();
      if (!node.IsEmpty()) {
        root.AddChild(std::move(node));
      }
    }
  }

  void Run() {
    root.Run(symbols);
  }

  void PrintSymbols() const { symbols.Print(); }

  void PrintAST(const ASTNode & node, std::string prefix="") const {
    std::cout << prefix << node.DebugString() << std::endl;
    for (const ASTNode & child : node.GetChildren()) {
      PrintAST(child, prefix+"  ");
    }
  }

  void PrintAST() const { PrintAST(root); }
};


int main(int argc, char * argv[])
{
  if (argc != 2) {
    std::cout << "Format: " << argv[0] << " [filename]" << std::endl;
    exit(1);
  }


  MACROCalc prog(argv[1]);
  prog.Parse();

  // prog.PrintSymbols();
  // prog.PrintAST();
  // std::cout << "---------" << std::endl;
  prog.Run();
  // std::cout << "---------" << std::endl;
  // prog.PrintSymbols();
}
