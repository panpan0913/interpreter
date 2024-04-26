#include "interpreter.h"
#include <sys/stat.h>

using namespace interpreter;

static const double DOUBLE_MAX = std::numeric_limits<double>::max();
static const double DOUBLE_MIN = std::numeric_limits<double>::min();

//输入表达式hash
const std::unordered_map<std::string, NonterminalExpression::ExpressionPath> InputExpression::hash = {
    {"SOURCE", {0, "source", 1, 1, {Commmon::name<TerminalExpression>()}}},
    {"INPUT", {1, "input", 2, 3, {Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>()}}},
    {"LABELS", {2, "labels", 2, 3, {Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>()}}},
    {"CELL", {3, "cell", 2, 2, {Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>()}}},
    {"WINDOW", {4, "clip", 5, 5, {Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>()}}},
};

//输出表达式hash
const std::unordered_map<std::string, NonterminalExpression::ExpressionPath> OutputExpression::hash = {
    {"OUTPUT", {0, "output", 2, 4, {Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>(), Commmon::name<TypicalExpression>()}}},
    {"TARGET", {1, "target", 1, 1, {Commmon::name<TerminalExpression>()}}},
    {"NEWTARGET", {2, "new_target", 1, 1, {Commmon::name<TerminalExpression>()}}},
    {"REPORT", {3, "report", 1, 2, {Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>()}}},
    {"NEWREPORT", {4, "new_report", 1, 2, {Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>()}}},
};

const std::unordered_map<std::string, NonterminalExpression::ExpressionPath> ComparatorExpression::hash = {
    {"==", {0, "==", 1, 1, {Commmon::name<TerminalExpression>()}}},
    {"!=", {1, "!=", 1, 1, {Commmon::name<TerminalExpression>()}}},
    {"<=", {3, ">=", 1, 1, {Commmon::name<TerminalExpression>()}}},
    {"<", {2, ">", 1, 1, {Commmon::name<TerminalExpression>()}}},
    {">=", {5, "<=", 1, 1, {Commmon::name<TerminalExpression>()}}},
    {">", {4, "<", 1, 1, {Commmon::name<TerminalExpression>()}}}
};

//
const std::unordered_map<std::string, NonterminalExpression::ExpressionPath> BYExpression::hash = {
    {"BY", {0, "", 1, 1, {typeid(TerminalExpression).name()}}}
};

//GROWhash
const std::unordered_map<std::string, NonterminalExpression::ExpressionPath> GrowOptionExpression::hash = {
    {"LEFT", {0, "", 1, 1, {typeid(BYExpression).name()}}},
    {"RIGHT", {1, "", 1, 1, {typeid(BYExpression).name()}}},
    {"TOP", {2, "", 1, 1, {typeid(BYExpression).name()}}},
    {"BOTTOM", {3, "", 1, 1, {typeid(BYExpression).name()}}}
};

const std::unordered_map<std::string, NonterminalExpression::ExpressionPath> SizeOptionExpression::hash = {
    {"OVERUNDER", {0, "1", 0, 0, {}}},
    {"UNDEROVER", {1, "2", 0, 0, {}}}
};

//LENGTHhash
const std::unordered_map<std::string, NonterminalExpression::ExpressionPath> LengthExpression::hash = {
    {"LENGTH1", {0, "length", 1, 1, {typeid(ComparatorExpression).name()}}},
    {"LENGTH2", {1, "length", 1, 1, {typeid(ComparatorExpression).name()}}},
    {"LENGTH", {2, "length", 1, 1, {typeid(ComparatorExpression).name()}}}
};

//CONVEXEDGEhash
std::unordered_map<std::string, NonterminalExpression::ExpressionPath> ConvexEdgeOptionExpression::hash = {
    {"ANGLE1", {0,"corners(as_edge_pairs)", 1, 2, {typeid(ComparatorExpression).name(), typeid(LengthExpression).name()}}},
    {"ANGLE2", {1, "corners(as_edge_pairs)", 1, 2, {typeid(ComparatorExpression).name(), typeid(LengthExpression).name()}}},
    {"WITH", {2, "", 1, 1, {typeid(LengthExpression).name()}}},
    {"EDGE", {3, "", 0, 0, {}}}
};

//EXTENThash
const std::unordered_map<std::string, NonterminalExpression::ExpressionPath> ExtentOptionExpression::hash = {
    {"EXTENTS", {0, "ext", 0, 0, {}}},
    {"EXTENDED", {1, "ext", 1, 1, {typeid(TerminalExpression).name()}}}
};

//RELATIONSOptionhash
const std::unordered_map<std::string, NonterminalExpression::ExpressionPath> RelationsOptionExpression::hash = {
    {"OPPOSITE", {0, "projection", 0, 1, {typeid(ExtentOptionExpression).name()}}},
    {"SQUARE", {1, "projection", 0, 0, {}}},
    {"SHILDED", {2, "shielded", 0, 0, {}}},
    {"PROJecting", {3, "projecting", 0, 1, {typeid(ComparatorExpression).name()}}},
    {"REGION",{4, "region", 0, 1, {typeid(ExtentOptionExpression).name()}}}
};

//LeftOptionhash
const std::unordered_map<std::string, NonterminalExpression::ExpressionPath> LeftOptionExpression::hash = {
    { "NOT", { 0, "PROJecting", 1, 1, {typeid(RelationsOptionExpression).name()} } },
    {"EXCLUDE", {1, "SHILDED", 1, 1, {typeid(RelationsOptionExpression).name()}}}
};


const std::vector<std::string> LogicalExpression::RelationOptionsList0 = {
typeid(Expression).name(),
typeid(ComparatorExpression).name(),
typeid(RelationsOptionExpression).name(),
typeid(RelationsOptionExpression).name(),
typeid(RelationsOptionExpression).name(),
typeid(RelationsOptionExpression).name()
};

const std::vector<std::string> LogicalExpression::RelationOptionsList1 = {
    typeid(Expression).name(),
    typeid(Expression).name(),
    typeid(ComparatorExpression).name(),
    typeid(RelationsOptionExpression).name(),
    typeid(RelationsOptionExpression).name(),
    typeid(RelationsOptionExpression).name(),
    typeid(RelationsOptionExpression).name()
};

const std::unordered_multimap<std::string, NonterminalExpression::ExpressionPath> LogicalExpression::hash = {
    {"AND", {1, "AND", 2, 2, {typeid(Expression).name(), typeid(ComparatorExpression).name()}}},
    {"AND", {0, "AND", 2, 2, {typeid(Expression).name(), typeid(Expression).name()}}},
    {"AND", {18, "AND", 1, 1, {typeid(Expression).name()}}},
    {"OR", {2, "OR", 1, 2, {typeid(Expression).name(), typeid(Expression).name()}}},
    {"XOR", {3, "XOR", 1, 2, {typeid(Expression).name(), typeid(Expression).name()}}},
    {"NOT", {4, "NOT", 2, 2, {typeid(Expression).name(), typeid(Expression).name()}}},
    {"ENCLOSE", {5, "covering", 2, 3, {typeid(Expression).name(), typeid(Expression).name(), typeid(ComparatorExpression).name()}}},
    {"INTERACT", {6, "interacting", 2, 3, {typeid(Expression).name(), typeid(Expression).name(), typeid(ComparatorExpression).name()}}},
    {"LENGTH", {7, "length", 2, 2, {typeid(Expression).name(), typeid(ComparatorExpression).name()}}},
    {"GROW", {8, "GROW", 2, 5, {typeid(Expression).name(),typeid(GrowOptionExpression).name(), typeid(GrowOptionExpression).name(), typeid(GrowOptionExpression).name(), typeid(GrowOptionExpression).name()}}},
    {"SIZE", {9, "SIZE", 2, 3, {typeid(Expression).name(), typeid(BYExpression).name(), typeid(SizeOptionExpression).name()}}},
    {"ANGLE", {10, "angle", 2, 2, {typeid(Expression).name(), typeid(ComparatorExpression).name()}}},
    {"CONVEX", {12, "CONVEX", 4, 5, {typeid(ConvexEdgeOptionExpression).name(), typeid(Expression).name(), typeid(ConvexEdgeOptionExpression).name(), typeid(ConvexEdgeOptionExpression).name(), typeid(ConvexEdgeOptionExpression).name()}}},
    {"CONVEX", {11, "CONVEX", 3, 4, {typeid(ConvexEdgeOptionExpression).name(), typeid(Expression).name(), typeid(ComparatorExpression).name(), typeid(ConvexEdgeOptionExpression).name()}}},
    {"ENCLOSURE", {13, "enclosed", 3, 7, LogicalExpression::RelationOptionsList1}},
    {"INTERNAL", {14, "overlap",  3, 7, LogicalExpression::RelationOptionsList1}},
    {"EXTERNAL", {15, "separation",  3, 7, LogicalExpression::RelationOptionsList1}},
    {"INTERNAL", {16, "width", 2, 6, LogicalExpression::RelationOptionsList0}},
    {"EXTERNAL", {17, "space", 2, 6, LogicalExpression::RelationOptionsList0}}
};

//parserFuncMap init
std::unordered_multimap<std::string, std::pair<ParserFunc, NonterminalExpression::ExpressionPath>> Parser::parserFuncMap = {};

//isCorrectExpressionMap init
std::unordered_map<std::string, std::function<bool(Expression*)>> Parser::isCorrectExpressionMap = {};

template<>
bool Parser::parser<ComparatorExpression>(const std::vector<std::string>& tokens, std::stack<std::shared_ptr<Expression>>& stack, int& i, const NonterminalExpression::ExpressionPath&)
{
    return CompoundComparatorExpression::parser(tokens, stack, i);
}

template<>
bool Parser::parser<LeftOptionExpression>(const std::vector<std::string>& tokens, std::stack<std::shared_ptr<Expression>>& stack, int& i, const NonterminalExpression::ExpressionPath& path)
{
    return LeftOptionExpression::parser(tokens, stack, i, path);
}

//
std::unordered_map<std::string, std::shared_ptr<InterPreterSingle>> Interpreter::interpreterMap = {};
bool Interpreter::isParserInit = false;

NonterminalExpression::NonterminalExpression(std::string op, int id, std::vector<std::shared_ptr<Expression>> childs) : op(op),
children(std::make_shared<std::vector<std::shared_ptr<Expression>>>(childs)), id(id) {}

std::string NonterminalExpression::interpret()
{
    std::ostringstream result;
    result << op;

    for (size_t i = 0; i < children->size(); i++)
    {
        if (i == 0)
        {
            result << "(";
        }
        result << children->at(i)->interpret();
        if (i != children->size() - 1)
        {
            result << ", ";
        }
        else
        {
            result << ")";
        }
    }

    return result.str();
};

//
ComparatorExpression::ComparatorExpression(std::string op, int id, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, id, childs), option(""), isDoubleDirection(false) {}

std::string ComparatorExpression::interpret()
{
    std::ostringstream result;
    double oper = std::stod(getOperand());
    if (!isReverse || isDoubleDirection)
    {
        result << option << op << std::fixed << std::setprecision(4) << oper;
    }

    if (isDoubleDirection)
    {
        result << "||";
    }

    if (isReverse || isDoubleDirection)
    {
        result << option << getReverseComparator() << "-" << std::fixed << std::setprecision(4) << oper;
    }

    return result.str();
}

// 判断是否为比较器
bool ComparatorExpression::isComparator(const std::string& str)
{
    return hash.find(str) != hash.end();
}

// 判断是否包含比较器
bool ComparatorExpression::containsComparator(const std::string& str)
{
    for (const auto& s : hash)
    {
        if (str.find(s.first) != std::string::npos)
        {
            return true;
        }
    }
    return false;
}

// 从字符串中获取比较器
std::string ComparatorExpression::getComparator(const std::string& str)
{
    for (auto& s : hash)
    {
        if (str.find(s.first) != std::string::npos)
        {
            return s.first;
        }
    }
    return "";
}

// 获取比较器的操作数
std::string ComparatorExpression::getOperand(const std::string& str)
{
    std::string comp = getComparator(str);
    return str.substr(comp.size());
}

// 设置option
void ComparatorExpression::setOption(std::string option)
{
    this->option = option;
}

// 获取option
std::string ComparatorExpression::getOption()
{
    return option;
}

// 获取比较器
std::string ComparatorExpression::getComparator()
{
    return op;
}

// 获取比较器的反向比较器
std::string ComparatorExpression::getReverseComparator() const
{
    return hash.at(op).condition;
}

// 获取比较器的value
int ComparatorExpression::getComparatorValue() const
{
    return hash.at(op).id;
}

// 获取操作数
std::string ComparatorExpression::getOperand()
{
    return children->at(0)->interpret();
}

// 设置是否为双向比较
void ComparatorExpression::setDoubleDirection(bool isDoubleDirection)
{
    this->isDoubleDirection = isDoubleDirection;
}

void interpreter::ComparatorExpression::setReverse()
{
    isReverse = !isReverse;
}

bool interpreter::ComparatorExpression::getReverse()
{
    return isReverse;
}

// 获取是否为双向比较
bool ComparatorExpression::getDoubleDirection() const
{
    return isDoubleDirection;
}

// 判断是否在范围内
bool ComparatorExpression::isInRange(double l, double g)
{
    bool res = false;
    double oper = std::stod(getOperand());
    switch (getId())
    {
    case 0:
        res = (l <= oper) && (g >= oper);
        break;
    case 1:
        res = (l <= oper) && (g > oper);
        break;
    case 2:
        res = (l < oper);
        break;
    case 3:
        res = (l <= oper);
        break;
    case 4:
        res = (g > oper);
        break;
    case 5:
        res = (g >= oper);
        break;
    default:
        break;
    }
    return res;
}

// 获取范围
std::tuple<double, double, bool> ComparatorExpression::getRange()
{
    double l = DOUBLE_MIN;
    double g = DOUBLE_MAX;
    switch (id)
    {
    case 0:
    case 1:
        l = g = std::stod(getOperand());
        break;
    case 2:
    case 3:
        g = std::stod(getOperand());
        break;
    case 4:
    case 5:
        l = std::stod(getOperand());
        break;
    default:
        break;
    }
    return std::make_tuple(l, g, id != 1);
}

// CompoundComparatorExpression
CompoundComparatorExpression::CompoundComparatorExpression(std::string op, std::vector<std::shared_ptr<Expression>> childs) : ComparatorExpression(op, -1, childs), le(DOUBLE_MIN), ge(DOUBLE_MAX), lt(DOUBLE_MIN), gt(DOUBLE_MAX), isRange(true)
{
    // 根据child的操作符类型设置ge, le, gt, lt, isRange
    for (auto&& child : childs)
    {
        ComparatorExpression* chi = static_cast<ComparatorExpression*>(child.get());
        double operand = std::stod(chi->getOperand());
        switch (chi->getComparatorValue())
        {
        case 0:
        {
            if (le <= operand && ge >= operand)
            {
                le = operand;
                ge = operand;
            }
            else
            {
                isRange = false;
            }
        }
        break;
        case 1:
        {
            if (lt <= operand && gt >= operand)
            {
                lt = operand;
                gt = operand;
            }
            else
            {
                isRange = false;
            }
        }
        break;
        case 2:
        {
            if (operand < le || operand < lt)
            {
                isRange = false;
            }
            gt = std::min(gt, operand);
        }
        break;
        case 3:
        {
            if (operand < le || operand < lt)
            {
                isRange = false;
            }
            ge = std::min(ge, operand);
        }
        break;
        case 4:
        {
            if (operand > ge || operand > gt)
            {
                isRange = false;
            }
            lt = std::max(lt, operand);
        }
        break;
        case 5:
        {
            if (operand > ge || operand > gt)
            {
                isRange = false;
            }
            le = std::max(le, operand);
        }
        break;
        default:
            break;
        }

        if (!isRange)
        {
            throw std::runtime_error("The comparator is not A range!");
        }
    }
}

std::string CompoundComparatorExpression::interpret()
{
    std::ostringstream result;
    if (isRange)
    {
        bool re = getReverse();
        bool isDouble = getDoubleDirection();
        double l = std::max(le, lt);
        std::string ls = (l == le) ? "<=" : "<";
        double g = std::min(ge, gt);
        std::string gs = (g == ge) ? "<=" : "<";
        std::string te = (le == ge) ? "==" : "!=";

        if (l == g)
        {
            if (!re || isDouble)
                result << getOption() << te << std::fixed << std::setprecision(4) << l;
            if (isDouble)
                result << " || ";
            if (re || isDouble)
                result << getOption() << te << std::fixed << std::setprecision(4) << -l;
        }
        else
        {
            if (!re || isDouble)
                result << std::fixed << std::setprecision(4) << l << ls << getOption() << gs << std::fixed << std::setprecision(4) << g;
            if (isDouble)
                result << " || ";
            if (re || isDouble)
                result << std::fixed << std::setprecision(4) << -g << gs << getOption() << ls << std::fixed << std::setprecision(4) << -l;
        }
    }
    else
    {
        throw std::runtime_error("The comparator is not A range!");
    }
    return result.str();
}

// 判断是否包含比较器
bool CompoundComparatorExpression::isThis(const std::string& str)
{
    return isComparator(str) || containsComparator(str);
}

// 解析比较器
bool CompoundComparatorExpression::parser(const std::vector<std::string>& tokens, std::stack<std::shared_ptr<Expression>>& stack, int& i)
{
    std::vector<std::shared_ptr<Expression>> childs;
    if (!stack.empty() && isComparator(tokens[i]))
    {
        childs.push_back(ParserComp(stack.top(), tokens[i]));
        stack.pop();
    }
    else if (containsComparator(tokens[i]))
    {
        childs.push_back(ParserComp(std::make_shared<TerminalExpression>(getOperand(tokens[i])), getComparator(tokens[i])));
    }
    while (i >= 2 && (isComparator(tokens[i - 2]) || containsComparator(tokens[i - 1])))
    {
        if (isComparator(tokens[i - 2]))
        {
            childs.push_back(ParserComp(std::make_shared<TerminalExpression>(tokens[i - 1]), tokens[i - 2]));
            i -= 2;
        }
        else
        {
            childs.push_back(ParserComp(std::make_shared<TerminalExpression>(getOperand(tokens[i - 1])), getComparator(tokens[i - 1])));
            i--;
        }
    }
    if (childs.size() > 1)
    {
        stack.push(std::make_shared<CompoundComparatorExpression>("Range", childs));
    }
    else if (childs.size() == 1)
    {
        stack.push(childs[0]);
    }
    else
    {
        return false;
    }
    return true;
}

// 解析比较器
std::shared_ptr<Expression> CompoundComparatorExpression::ParserComp(const std::shared_ptr<Expression>& exp, const std::string& str)
{
    std::vector<std::shared_ptr<Expression>> childs({ exp });
    return std::make_shared<ComparatorExpression>(str, ComparatorExpression::hash.at(str).id, childs);
}

// 判断是否在范围内
bool CompoundComparatorExpression::isInRange(double l, double g)
{
    return isRange && ((l <= le || l <= lt) || (g >= ge || g >= gt));
}

// 获取范围
std::tuple<double, double, bool> CompoundComparatorExpression::getRange()
{
    return std::make_tuple(std::max(le, lt), std::min(ge, gt), id != 1);
}


//输入表达式hash
/* const std::unordered_map<std::string, NonterminalExpression::ExpressionPath> InputExpression::hash = {
    {"SOURCE", {0, "source", 1, 1, {Commmon::name<TerminalExpression>()}}},
    {"INPUT", {1, "input", 2, 3, {Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>()}}},
    {"LABELS", {2, "labels", 2, 3, {Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>()}}},
    {"CELL", {3, "cell", 2, 2, {Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>()}}},
    {"WINDOW", {4, "clip", 5, 5, {Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>()}}},
};

//输出表达式hash
const std::unordered_map<std::string, NonterminalExpression::ExpressionPath> OutputExpression::hash = {
    {"OUTPUT", {0, "output", 2, 4, {Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>(), Commmon::name<TypicalExpression>()}}},
    {"TARGET", {1, "target", 1, 1, {Commmon::name<TerminalExpression>()}}},
    {"NEWTARGET", {2, "new_target", 1, 1, {Commmon::name<TerminalExpression>()}}},
    {"REPORT", {3, "report", 1, 2, {Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>()}}},
    {"NEWREPORT", {4, "new_report", 1, 2, {Commmon::name<TerminalExpression>(), Commmon::name<TerminalExpression>()}}},
}; */

std::string interpreter::InputExpression::interpret()
{
    std::ostringstream result;
    auto con = hash.find(op)->second.condition;
    switch (id)
    {
        case 0:
            result << con << "(\"" << children->at(0)->interpret() << "\")";
            break;

        case 1:
        case 2:
            result << children->at(0)->interpret() << "." << con << "(" << children->at(1)->interpret();
            if (children->size() == 3)
            {
                result << ", " << children->at(2)->interpret();
            }
            result << ")";
            break;
        
        case 3:
            result << children->at(0)->interpret() << "." << con << "(" << children->at(1)->interpret() << ")";
            break;
        
        case 4:
            result << children->at(0)->interpret() << "." << con << "(p(" << children->at(1)->interpret() << ", " << children->at(2)->interpret() << "), p(" << children->at(3)->interpret() << ", " << children->at(4)->interpret() << "))";
            break;
        default:
            break;
    }
    return result.str();
}

std::string interpreter::OutputExpression::interpret()
{
    std::ostringstream result;
    auto con = hash.find(op)->second.condition;
    switch (id)
    {
        case 0:
            result << children->at(0)->interpret() << "." << con << "(" << children->at(1)->interpret();
            for (size_t i = 2; i < children->size(); i++)
            {
                result << ", " << children->at(i)->interpret();
            }
            result << ")";
            break;

        case 1:
        case 2:
            result << con << "(\"" << children->at(0)->interpret() << "\")";
            break;
        case 3:
        case 4:
            result << children->at(0)->interpret() << "." << con << "(\"" << children->at(1)->interpret()<<"\"";
            if (children->size() == 2)
            {
                result << ")";
            }
            else
            {
                result << ", \"" << children->at(2)->interpret() << "\")";
            }
            break;
        default:
            break;
    }
    return result.str();
}

// LogicalExpression
LogicalExpression::LogicalExpression(std::string op, int d, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, d, childs) {}

std::string LogicalExpression::interpret()
{
    std::ostringstream result;
    switch (id)
    {
    case 0:
        result << NonterminalExpression::interpret();
        break;
    case 1:
        ANDSelfInterpreter(result);
        break;
    case 2:
        if (children->size() == 1)
        {
            result << "OR_SELF(" << children->at(0)->interpret() << ")";
        }
        else
        {
            result << NonterminalExpression::interpret();
        }
        break;
    case 3:
        if (children->size() == 1)
        {
            result << "XOR_SELF(" << children->at(0)->interpret() << ")";
        }
        else
        {
            result << NonterminalExpression::interpret();
        }
        break;
    case 4:
        result << NonterminalExpression::interpret();
        break;
    case 5:
    case 6:
    {
        setOption(2, false, true);
        result << NonterminalExpression::interpret();
    }
    break;
    case 7:
    {
        setOption(1);
        result << NonterminalExpression::interpret();
    }
    break;
    case 8:
        GrowInterpreter(result);
        break;
    case 9:
        result << NonterminalExpression::interpret();
        break;
    case 10:
    {
        setOption(1, true);
        result << NonterminalExpression::interpret();
    }
    break;
    case 11:
        ConvexInterpreter(result);
        break;
    case 12:
        ConvexDetailInterpreter(result);
        break;
    case 13:
    case 14:
    case 15:
        RelationsInterpreter(result, 1);
        break;
    case 16:
    case 17:
        RelationsInterpreter(result, 0);
        break;
    case 18:
        ANDSelfInterpreter(result);
        break;

    default:
        break;
    }
    return result.str();
}

void interpreter::LogicalExpression::RelationsInterpreter(std::ostringstream& result, int mode)
{
    bool isDoubleMode = (mode == 1);
    result << op << (isDoubleMode ? "( " : "_SELF(") << children->at(0)->interpret() << ", ";
    std::ostringstream strCon;
    bool isRegion = false;
    double ext = 0;
    strCon << GetCondition() << "(" << (isDoubleMode ? children->at(1)->interpret() : "");
    ComparatorExpression* comp = dynamic_cast<ComparatorExpression*>(children->at(mode + 1).get());
    for (int i = 2 + mode; i < children->size(); i++)
    {
        RelationsOptionExpression* chi = static_cast<RelationsOptionExpression*>(children->at(i).get());
        switch (chi->getId())
        {
        case 0:
        {
            auto cc = chi->getChildren();
            if (cc->size() == 1)
            {
                ext = std::stod(cc->at(0)->interpret());
            }
            strCon << (isDoubleMode ? ", " : "") << chi->interpret();
            isDoubleMode = true;
        }
        break;
        case 1:
        {
            strCon << (isDoubleMode ? ", " : "") << chi->interpret();
            isDoubleMode = true;
            ext = std::stod(comp->getOperand());
        }
        break;
        case 2:
        {
            strCon << (isDoubleMode ? ", " : "") << chi->interpret();
            isDoubleMode = true;
        }
        break;
        case 3:
        {
            if (chi->interpret() != "NOT")
            {
                strCon << (isDoubleMode ? ", " : "") << chi->interpret();
                isDoubleMode = true;
            }
        }
        break;
        case 4:
        {
            isRegion = true;
            if (chi->interpret() != "REGION")
            {
                ext = std::stod(chi->interpret());
            }
        }
        break;
        default:
            break;
        }
    }
    strCon << ")";
    comp->setOption(strCon.str());
    result << comp->interpret();

    if (isRegion || ext != 0)
    {
        result << ", " << isRegion ? "true" : "false";
    }

    if (ext != 0)
    {
        result << ", " << ext;
    }

    result << ")";
}

void interpreter::LogicalExpression::ConvexDetailInterpreter(std::ostringstream& result)
{
    result << op << children->at(0)->interpret() << "_DETAIL(";
    for (size_t i = 1; i < children->size(); i++)
    {
        result << children->at(i)->interpret();
        if (i != children->size() - 1)
        {
            result << ", ";
        }
    }
    result << ")";
}

void interpreter::LogicalExpression::ConvexInterpreter(std::ostringstream& result)
{
    result << op << children->at(0)->interpret() << "(" << children->at(1)->interpret() << ", ";
    ComparatorExpression* chi = static_cast<ComparatorExpression*>(children->at(2).get());
    if (chi->isInRange(0, 1.5))
    {
        result << "1";
    }
    else if (chi->isInRange(1.5, 2.5))
    {
        result << "2";
    }
    else
    {
        throw std::runtime_error("The expression is not supported!");
    }

    if (children->size() == 4)
    {
        result << "," << children->at(3)->interpret();
    }
    result << ")";
}

void interpreter::LogicalExpression::GrowInterpreter(std::ostringstream& result)
{
    std::vector<double> dir(4, 0.0);
    result << op << "(" << children->at(0)->interpret() << ", ";
    for (size_t i = 1; i < children->size(); i++)
    {
        GrowOptionExpression* chi = static_cast<GrowOptionExpression*>(children->at(i).get());
        int id = chi->getId();
        dir[id] = std::stod(chi->interpret());
    }

    for (size_t i = 0; i < 4; i++)
    {
        result << std::fixed << std::setprecision(4) << dir[i];
        if (i != 3)
        {
            result << ", ";
        }
    }
    result << ")";
}

void interpreter::LogicalExpression::setOption(int c, bool r, bool d)
{
    if (children->size() > c)
    {
        auto myOp = GetCondition();
        myOp += d ? ("(" + children->at(c-1)->interpret() + ")") : "";
        ComparatorExpression* chi = static_cast<ComparatorExpression*>(children->at(c).get());
        chi->setOption(myOp);
        chi->setDoubleDirection(r);
    }
}

const std::string& interpreter::LogicalExpression::GetCondition() const
{
    for (auto it = hash.find(op); it != hash.end(); it++)
    {
        if (it->second.id == id)
        {
            return it->second.condition;
        }
    }
    return op;
}

// AND运算符的特殊处理
void LogicalExpression::ANDSelfInterpreter(std::ostringstream& result)
{
    result << "AND_SELF(" << children->at(0)->interpret();
    if (children->size() == 1)
    {
        result << ")";
        return;
    }
    result << ", ";
    ComparatorExpression* chi = static_cast<ComparatorExpression*>(children->at(1).get());
    if (chi->isInRange(1, DOUBLE_MAX))
    {
        auto lgr = chi->getRange();
        // 获取tuple的值
        double l = std::get<0>(lgr);
        double g = std::get<1>(lgr);
        bool r = std::get<2>(lgr);
        int min = std::max(l, 2.0);
        int max = std::min(g, 10.0);
        bool re = r;
        result << min;
        if (!re)
        {
            result << ", " << max << ", "
                << "false";
        }
        else if (max < 10 && max != min)
        {
            result << ", " << max;
        }
        result << ")";
    }
}

std::string ConvexEdgeOptionExpression::interpret()
{
    std::ostringstream result;
    int tid = getId();
    if (tid < 2)
    {
        ComparatorExpression* chi = static_cast<ComparatorExpression*>(children->at(0).get());
        chi->setReverse();
        chi->setOption(hash.at(op).condition);
        if (children->size() == 2)
        {
            LengthExpression* chi2 = reinterpret_cast<LengthExpression*>(children->at(1).get());
            ComparatorExpression* cc = dynamic_cast<ComparatorExpression*>(chi2->getChildren()->at(0).get());
            std::string myOp = "(" + chi->interpret() + ")";
            cc->setOption(myOp);
            result << cc->interpret();
        }
        else
        {
            result << chi->interpret();
        }
    }
    else if (tid == 2)
    {
        result << children->at(0)->interpret();
    }
    else
    {
        result << op;
    }
    return result.str();
}

bool LeftOptionExpression::parser(const std::vector<std::string>& tokens, std::stack<std::shared_ptr<Expression>>& stack, int& i, const ExpressionPath& path)
{
    if (stack.size() >= 1)
    {
        NonterminalExpression* exp = dynamic_cast<NonterminalExpression*>(stack.top().get());
        if (exp && exp->getOp() == path.condition)
        {
            std::vector<std::shared_ptr<Expression>> childs;
            exp->getChildren()->push_back(std::make_shared<LeftOptionExpression>(tokens[i], path.id, childs));
        }
        else
        {
            return false;
        }
    }
    else
    {
        return false;
    }
    return true;
}

RelationsOptionExpression::RelationsOptionExpression(std::string str, int id, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(str, id, childs) {}

std::string RelationsOptionExpression::interpret()
{
    std::ostringstream result;
    switch (getId())
    {
    case 3:
    {
        if (children->size() == 0)
        {
            result << hash.at(op).condition << ">0";
        }
        else if (Commmon::isInstance<LeftOptionExpression>(children->at(0).get()))
        {
            result << children->at(0)->interpret() << op;
        }
        else if (Commmon::isInstance<ComparatorExpression>(children->at(0).get()))
        {
            ComparatorExpression* chi = static_cast<ComparatorExpression*>(children->at(0).get());
            chi->setOption(hash.at(op).condition);
            result << chi->interpret();
        }
        else
        {
            throw std::runtime_error("The expression is not supported!");
        }
    }
    break;
    case 4:
    {
        if (children->size() == 0)
        {
            result << op;
        }
        else
        {
            result << children->at(0)->interpret();
        }
    }
    break;
    default:
        result << hash.at(op).condition;
        break;
    }
    return result.str();
}

std::vector<std::string> Parser::tokenize(const std::string& str)
{
    std::vector<std::string> tokens;
    std::istringstream iss(str);
    std::string token;
    while (iss >> token)
    {
        tokens.push_back(token);
    }
    return tokens;
}

std::shared_ptr<Expression> Parser::parse(std::string& str)
{
    str.erase(std::remove(str.begin(), str.end(), '\r'), str.end());
    if (str.empty())
    {
        return std::make_shared<TerminalExpression>("");
    }

    // 正则检查输入为英文字符，数字，空格，符号, 括号
    std::regex reg("^[a-zA-Z0-9\\s\\(\\)\\[\\]\\{\\}\\+\\-\\*\\/\\=\\<\\>\\!\\&\\|\\^\\~\\%\\@\\#\\$\\?\\:\\;\\,\\.\\'\\\"\\`\\_\\|\\&\\^\\~\\%\\@\\#\\$\\?\\:\\;\\,\\.\\'\\\"\\`\\_]+$");
    if (!std::regex_match(str, reg))
    {
        throw std::runtime_error("The expression is not supported!");
    }
    return parse(tokenize(str));
}

auto Parser::getIter(std::string str)
{
    auto it = parserFuncMap.equal_range(str);
    if (it.first != parserFuncMap.end())
    {
        return it;
    }
    else
    {
        auto s = ComparatorExpression::getComparator(str);
        return parserFuncMap.equal_range(s);
    }
}

// 类似前缀表达式的解析，实现递归解析
std::shared_ptr<Expression> Parser::parse(const std::vector<std::string>& tokens)
{
    std::stack<std::shared_ptr<Expression>> stack;
    for (int i = tokens.size() - 1; i >= 0; i--)
    {
        bool isParsed = false;
        auto range = getIter(tokens[i]);
        int j = 0;
        for (auto it = range.first; it != range.second; it++)
        {
            isParsed |= it->second.first(tokens, stack, i, it->second.second);
            if (isParsed)
            {
                break;
            }
        }
        if (!isParsed)
        {
            stack.push(std::make_shared<TerminalExpression>(tokens[i]));
        }
    }
    if (stack.size() == 1)
    {
        return stack.top();
    }
    else
    {
        std::string str = stack.top()->interpret();
        stack.pop();
        while (!stack.empty())
        {
            str += " " + stack.top()->interpret();
            stack.pop();
        }
        return std::make_shared<TerminalExpression>(str);
    }
}

template <typename T>
bool Parser::parser(const std::vector<std::string>& tokens, std::stack<std::shared_ptr<Expression>>& stack, int& i, const NonterminalExpression::ExpressionPath& path)
{
    std::vector<std::shared_ptr<Expression>> childs;
    int min = path.minOptions;
    int max = path.maxOptions;
    auto& pTypes = path.optionTypes;
    int j = 0;

    if (stack.size() < min)
    {
        return false;
    }

    for (; j < min; j++)
    {
        if (isCorrectExpressionMap.at(pTypes.at(j))(stack.top().get()))
        {
            childs.push_back(stack.top());
            stack.pop();
        }
        else
        {
            for (int k = j - 1; k >= 0; k--)
            {
                stack.push(childs[k]);
            }
            return false;
        }
    }

    while (j < max && stack.size() > 0 && isCorrectExpressionMap.at(pTypes.at(j))(stack.top().get()))
    {
        childs.push_back(stack.top());
        stack.pop();
        j++;
    }

    stack.push(std::make_shared<T>(tokens[i], path.id, childs));
    return true;
}

template<typename T>
void Parser::addCorrectExpressionMap()
{
    isCorrectExpressionMap.insert({ typeid(T).name(), Commmon::isInstance<T> });
}

template <typename T>
void Parser::Register2Parser()
{
    addCorrectExpressionMap<T>();
    for (auto& p : T::hash)
    {
        parserFuncMap.insert({ p.first, {parser<T>, p.second} });
    }
}

// InterPreter
InterPreterSingle::InterPreterSingle(const std::string& filename) : path(filename) {}

InterPreterSingle::~InterPreterSingle()
{
    stop();
}

// 将字符串数组按行解析成表达式数组，并将表达式数组按行解析成字符串数组，解析过程中捕获异常，并打印行号和异常信息
auto InterPreterSingle::parseLines()
{
    std::vector<std::shared_ptr<Expression>> result;
    bool bOK = true;
    for (size_t i = 0; i < lines.size(); i++)
    {
        try
        {
            result.push_back(Parser::parse(lines[i]));
        }
        catch (const std::exception& e)
        {
            bOK = false;
            errStr = "Line " + std::to_string(i) + ": " + e.what();
            std::cerr << "Line " << i << ": " << e.what() << std::endl;
        }
    }
    isSuccessful = bOK;
    return result;
}

std::string InterPreterSingle::Start()
{
    lines = readLines();
    thread = std::thread(&InterPreterSingle::run, this);
    setRuningState(1);
    return "OK";
}

// 暂停线程，使线程不再占用资源
void InterPreterSingle::Pause()
{
    std::unique_lock<std::mutex> lock(mtx);
    paused = true;
    setRuningState(2);
}

// 继续线程
void InterPreterSingle::resume()
{
    std::unique_lock<std::mutex> lock(mtx);
    paused = false;
    setRuningState(1);
    cv.notify_all();
}

// 停止线程
void InterPreterSingle::stop()
{
    {
        std::unique_lock<std::mutex> lock(mtx);
        paused = false;
        stopped = true;
        setRuningState(0);
        cv.notify_all();
    }
    if (thread.joinable())
    {
        thread.join();
        thread.~thread();
    }
}

// run
void InterPreterSingle::run()
{
    bool bOK = true;
    for (int i = 0; i < lines.size(); i++)
    {
        {
            std::unique_lock<std::mutex> lock(mtx);
            if (breakLine == i)
            {
                breakLine = -1;
                paused = true;
                isBreak = true;
                cv.notify_all();
				setRuningState(2);
            }
            else if (breakLine < i && breakLine != -1)
            {
                isBreak = true;
                breakLine = -1;
                cv.notify_all();
            }
        }
        {
            //std::cout << "paused: " << paused << std::endl;
            std::unique_lock<std::mutex> lock(mtx);
            cv.wait(lock, [this] { return !paused; });
        }

        try
        {
            expressions.push_back(Parser::parse(lines[i]));
        }
        catch (const std::exception& e)
        {
            bOK = false;
            errStr += "Line " + std::to_string(i + 1) + ": " + e.what() + "\n";
            std::cerr << "Line " << i + 1 << ": " << e.what() << std::endl;
        }
    }
    if (bOK)
    {
        try
        {
            write();
            while (!stopped)
            {
                {
                    //std::cout << "paused: " << paused << std::endl;
                    std::unique_lock<std::mutex> lock(mtx);
                    cv.wait(lock, [this] { return !paused; });
                }
                char * klayoutPath = getenv("KLAYOUT_PATH");
                std::cout << klayoutPath << std::endl;
                std::string klayoutrun = "/home/zhangpan/桌面/klayout/bin/klayout -b -r " + path + "_KL.drc";
                //std::string klayoutrun = "sudo "+ std::string(klayoutPath) + "/klayout -b -r tp.drc";
                std::cout << klayoutrun << std::endl;
                int result = system(klayoutrun.c_str());
                if (result != 0)
                {
                    std::cerr << "Failed to run klayout command: " << klayoutrun << '\n';
                }
                isSuccessful = true;
                {
                    std::unique_lock<std::mutex> lock(mtx);
                    paused = true;
                    std::cout << "setRuningState(0)" << std::endl;
                    setRuningState(0);
                }
            }
        }
        catch (const std::exception& e)
        {
            std::cerr << e.what() << '\n';
        }
    }
}

// 根据输入文件名读取一个文本文件
std::string InterPreterSingle::read()
{
    std::string s = path + ".drc";
    std::ifstream file(s);
    if (!file.is_open())
    {
        std::string err = "Line 950: The file " + s + " is not found!";
        throw std::runtime_error(err.c_str());
    }
    std::string str((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    return str;
}

// 根据输入文件名写一个文本文件
void InterPreterSingle::write()
{
    std::string s = path + "_KL.drc";
    std::ofstream file(s);
    if (!file.is_open())
    {
        std::string err = "Line 961: The file " + s + " is not found!";
        throw std::runtime_error(err.c_str());
    }
    auto ls = std::vector<std::string>();
    for (size_t i = 0; i < expressions.size(); i++)
    {
        ls.push_back(expressions[i]->interpret());
    }
    std::string out = "# %include drcrule.drc\n\n" + join(ls);
    file << out;
    chmod(s.c_str(), 0777);
}

// 根据输入文件名读取一个文本文件,并将其分割成一个字符串数组
std::vector<std::string> InterPreterSingle::readLines()
{
    return split(read());
}

// 将一个多行字符串分割成一个字符串数组
std::vector<std::string> InterPreterSingle::split(const std::string& str)
{
    std::vector<std::string> result;
    std::istringstream iss(str);
    std::string line;
    while (std::getline(iss, line))
    {
        result.push_back(line);
    }
    return result;
}

// 将一个字符串数组合并成一个多行字符串
std::string InterPreterSingle::join(const std::vector<std::string>& lines)
{
    std::ostringstream oss;
    for (size_t i = 0; i < lines.size(); i++)
    {
        oss << lines[i] << std::endl;
    }
    return oss.str();
}

void InterPreterSingle::SetBreakPoint(int line)
{
    std::unique_lock<std::mutex> lock(mtx);
    breakLine = line;
    isBreak = false;
    paused = false;
    cv.notify_all();
}

// InterPreter init

void Interpreter::ParserInit()
{
    //no hash class
    Parser::addCorrectExpressionMap<Expression>();
    Parser::addCorrectExpressionMap<TerminalExpression>();
    Parser::addCorrectExpressionMap<NonterminalExpression>();
    Parser::addCorrectExpressionMap<CompoundComparatorExpression>();

    //hash class
    Parser::Register2Parser<ComparatorExpression>();
    Parser::Register2Parser<GrowOptionExpression>();
    Parser::Register2Parser<SizeOptionExpression>();
    Parser::Register2Parser<BYExpression>();
    Parser::Register2Parser<LengthExpression>();
    Parser::Register2Parser<ConvexEdgeOptionExpression>();
    Parser::Register2Parser<ExtentOptionExpression>();
    Parser::Register2Parser<RelationsOptionExpression>();
    Parser::Register2Parser<LeftOptionExpression>();
    Parser::Register2Parser<LogicalExpression>();
    Parser::Register2Parser<InputExpression>();
    Parser::Register2Parser<OutputExpression>();
}

void Interpreter::Init()
{
    if (!isParserInit)
    {
        ParserInit();
        isParserInit = true;
    }
}

// InterPreter add
std::string Interpreter::addInterpreter(const std::string& path, SetRuningState setRun)
{
    std::shared_ptr<InterPreterSingle> interPreter = std::make_shared<InterPreterSingle>(path);
    interPreter->setRuningState = setRun;
    interpreterMap[path] = interPreter;
    return interPreter->Start();
}

void Interpreter::removeInterpreter(const std::string& path)
{
    interpreterMap.erase(path);
}

std::string Interpreter::Run(const std::string& path, SetRuningState setRun)
{
    Init();
    auto it = interpreterMap.find(path);
    if (it != interpreterMap.end())
    {
        it->second->resume();
        return "OK";
    }
    else
    {
        return addInterpreter(path, setRun);
    }
}

void Interpreter::Pause(const std::string& path)
{
    auto it = interpreterMap.find(path);
    if (it != interpreterMap.end())
    {
        it->second->Pause();
    }
}

void Interpreter::Resume(const std::string& path)
{
    auto it = interpreterMap.find(path);
    if (it != interpreterMap.end())
    {
        it->second->resume();
    }
}

void Interpreter::Stop(const std::string& path)
{
    auto it = interpreterMap.find(path);
    if (it != interpreterMap.end())
    {
        it->second->stop();
    }
    removeInterpreter(path);
}

void Interpreter::SetBreakPoint(const std::string& path, int line)
{
    auto it = interpreterMap.find(path);
    if (it != interpreterMap.end())
    {
        it->second->SetBreakPoint(line);
    }
}

bool Interpreter::getIsBreak(const std::string& path)
{
    auto it = interpreterMap.find(path);
    if (it != interpreterMap.end())
    {
        return it->second->getIsBreak();
    }
    return false;
}

int Interpreter::getRunTimes(const std::string& path)
{
    auto it = interpreterMap.find(path);
    if (it != interpreterMap.end())
    {
        return it->second->getRunTimes();
    }
    return 0;
}

bool Interpreter::getPaused(const std::string& path)
{
    auto it = interpreterMap.find(path);
    if (it != interpreterMap.end())
    {
        return it->second->getPaused();
    }
    return false;
}

int Interpreter::getBreakLine(const std::string& path)
{
    auto it = interpreterMap.find(path);
    if (it != interpreterMap.end())
    {
        return it->second->getBreakLine();
    }
    return -1;
}

// 主函数
int main()
{
    //test
    Interpreter::Init();
    std::cout << "Test Start!" << std::endl;
    Interpreter::Run("test", [](int state) { std::cout << "State: " << state << std::endl; });
    std::cout << "Test Pause!" << std::endl;
    Interpreter::Pause("test");
    std::cout << "Test SetBreakPoint!" << std::endl;
    Interpreter::SetBreakPoint("test", 2);
    std::cout << "Test Resume!" << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(2));
    while (!Interpreter::getIsBreak("test"))
    {
        std::this_thread::sleep_for(std::chrono::seconds(1));
    }

    std::this_thread::sleep_for(std::chrono::seconds(10));
    Interpreter::Stop("test");
    return 0;
}

