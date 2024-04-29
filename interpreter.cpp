#include "interpreter.h"
#include <sys/stat.h>

using namespace interpreter;

static const double DOUBLE_MAX = std::numeric_limits<double>::max();
static const double DOUBLE_MIN = std::numeric_limits<double>::min();

//parserFuncMap init
std::unordered_multimap<std::string, std::pair<ParserFunc, std::shared_ptr<PreExpressionInfo>>> Parser::parserFuncMap = {};

template<>
bool Parser::parser<ComparatorExpression>(const std::vector<std::string>& tokens, std::stack<std::shared_ptr<Expression>>& stack, int& i, PreExpressionInfo&)
{
    return CompoundComparatorExpression::parser(tokens, stack, i);
}

template<>
bool Parser::parser<LeftOptionExpression>(const std::vector<std::string>& tokens, std::stack<std::shared_ptr<Expression>>& stack, int& i, PreExpressionInfo& path)
{
    return LeftOptionExpression::parser(tokens, stack, i, path);
}

//
std::unordered_map<std::string, std::shared_ptr<InterPreterSingle>> Interpreter::interpreterMap = {};
bool Interpreter::isParserInit = false;

NonterminalExpression::NonterminalExpression(std::string op, NonTerminalExpressionType id, std::vector<std::shared_ptr<Expression>> childs) : op(op),
children(std::make_shared<std::vector<std::shared_ptr<Expression>>>(childs)), nType(id) {}

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

//hash init
std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> ComparatorExpression::hash = {};

std::vector<isMatchFunc> ExpressionOptionLimit::isMatchFuncs={};

void ExpressionOptionLimit::init()
{
    isMatchFuncs.push_back(isExpTypeMatch);
    isMatchFuncs.push_back(isNExpTypeMatch);
    isMatchFuncs.push_back(isExpCountMatch);
    isMatchFuncs.push_back(isNumberMatch);
    isMatchFuncs.push_back(isNExp1InNMatch);
}

bool ExpressionOptionLimit::isMatch(Expression* exp, const std::vector<std::shared_ptr<Expression>>& childs)
{
    bool res = true;
    for (int i = 0; i < 5; i++)
    {
        if (limitType & (1 << i))
        {
            res &= isMatchFuncs.at(i)(exp, childs, limitValue[1<<i]);
        }
    }
    return res;
}

//
bool ExpressionOptionLimit::isExpTypeMatch(Expression* exp, const std::vector<std::shared_ptr<Expression>>& childs, std::any val)
{
    return exp->getType() == std::any_cast<ExpressionType>(val);
}

bool ExpressionOptionLimit::isNExpTypeMatch(Expression* exp, const std::vector<std::shared_ptr<Expression>>& childs, std::any val)
{
    NonterminalExpression *nexp = dynamic_cast<NonterminalExpression*>(exp);
    return nexp && nexp->getNType() == std::any_cast<NonTerminalExpressionType>(val);
}

bool ExpressionOptionLimit::isExpCountMatch(Expression* exp, const std::vector<std::shared_ptr<Expression>>& childs, std::any val)
{
    int count = std::any_cast<int>(val);
    NonterminalExpression *nexp = dynamic_cast<NonterminalExpression*>(exp);
    if (!nexp)
    {
        return false;//非非终结符, 本限制只针对非终结符
    }
    std::string op = nexp->getOp();
    
    for (auto& ch: childs)
    {
        NonterminalExpression *child = dynamic_cast<NonterminalExpression*>(ch.get());
        if (child && child->getOp() == op)
        {
            count--;
        }
    }
    
    return count > 0;
}

bool ExpressionOptionLimit::isNumberMatch(Expression* exp, const std::vector<std::shared_ptr<Expression>>& childs, std::any val)
{
    std::string str = exp->interpret();
    try {
        size_t pos;
        double val = std::stod(str, &pos);
        return pos == str.size();
    } catch (std::invalid_argument& e) {
        return false;
    } catch (std::out_of_range& e) {
        return false;
    }
}

bool ExpressionOptionLimit::isNExp1InNMatch(Expression* exp, const std::vector<std::shared_ptr<Expression>>& childs, std::any val)
{
    NonterminalExpression *nexp = dynamic_cast<NonterminalExpression*>(exp);
    if (!nexp)
    {
        return false;//非非终结符, 本限制只针对非终结符
    }
    NonTerminalExpressionType op = nexp->getNType();
    std::vector<NonTerminalExpressionType> ntypes = std::any_cast<std::vector<NonTerminalExpressionType>>(val);
    if (std::find(ntypes.begin(), ntypes.end(), op) == ntypes.end())
    {
        return true; //表达式不在组内直接返回成功
    }
    
    for (auto&& child : childs)
    {
        NonterminalExpression *childExp = dynamic_cast<NonterminalExpression*>(child.get());
        if (childExp && std::find(ntypes.begin(), ntypes.end(), childExp->getNType()) != ntypes.end())
        {
            return false;//有一个匹配就返回失败
        }
    }

    return true;
}

// Fix hash
std::unordered_map<std::string, ExpressionType> FixExpression::hash = {};

//inithash
void FixExpression::initHash()
{
    hash.insert({ "=", ExpressionType::ASSIGN});
}

//inithash
void ComparatorExpression::initHash()
{
    auto mnLimit = LIMIT(true, EXPRESSIONNUMBERLIMITFLAG, {EXPRESSIONNUMBERLIMITFLAG, ExpressionType::NUMBER});

    hash.insert({ "==", PREPTR("==", ExpressionType::CONSTRAINT, NonTerminalExpressionType::EQUAL, "==", 1, 1, LIMITLIST(mnLimit)) });
    hash.insert({ "!=", PREPTR("!=", ExpressionType::CONSTRAINT, NonTerminalExpressionType::NOT_EQUAL, "!=", 1, 1, LIMITLIST(mnLimit))});
    hash.insert({ "<=", PREPTR("<=", ExpressionType::CONSTRAINT, NonTerminalExpressionType::LESS_EQUAL, "<=", 1, 1, LIMITLIST(mnLimit)) });
    hash.insert({ "<", PREPTR("<", ExpressionType::CONSTRAINT, NonTerminalExpressionType::LESS, "<", 1, 1, LIMITLIST(mnLimit)) });
    hash.insert({ ">=", PREPTR(">=", ExpressionType::CONSTRAINT, NonTerminalExpressionType::GREATER_EQUAL, ">=", 1, 1, LIMITLIST(mnLimit)) });
    hash.insert({ ">", PREPTR(">", ExpressionType::CONSTRAINT, NonTerminalExpressionType::GREATER, ">", 1, 1, LIMITLIST(mnLimit)) });

    hash["=="]->setReverse(hash["!="]);
    hash["!="]->setReverse(hash["=="]);
    hash["<="]->setReverse(hash[">="]);
    hash["<"]->setReverse(hash[">"]);
    hash[">="]->setReverse(hash["<="]);
    hash[">"]->setReverse(hash["<"]);
}

//
ComparatorExpression::ComparatorExpression(std::string op, NonTerminalExpressionType id, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, id, childs), option(""), isDoubleDirection(false) {setType(ExpressionType::CONSTRAINT);}

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
    return hash.at(op)->getReverse()->getName();
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
    switch (getNType())
    {
    case NonTerminalExpressionType::EQUAL:
        res = (l <= oper) && (g >= oper);
        break;
    case NonTerminalExpressionType::NOT_EQUAL:
        res = (l <= oper) && (g > oper);
        break;
    case NonTerminalExpressionType::LESS:
        res = (l < oper);
        break;
    case NonTerminalExpressionType::LESS_EQUAL:
        res = (l <= oper);
        break;
    case NonTerminalExpressionType::GREATER:
        res = (g > oper);
        break;
    case NonTerminalExpressionType::GREATER_EQUAL:
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
    switch (getNType())
    {
    case NonTerminalExpressionType::EQUAL:
    case NonTerminalExpressionType::NOT_EQUAL:
        l = g = std::stod(getOperand());
        break;
    case NonTerminalExpressionType::LESS:
    case NonTerminalExpressionType::LESS_EQUAL:
        g = std::stod(getOperand());
        break;
    case NonTerminalExpressionType::GREATER:
    case NonTerminalExpressionType::GREATER_EQUAL:
        l = std::stod(getOperand());
        break;
    default:
        break;
    }
    return std::make_tuple(l, g, getNType() != NonTerminalExpressionType::NOT_EQUAL);
}

// CompoundComparatorExpression
CompoundComparatorExpression::CompoundComparatorExpression(std::string op, std::vector<std::shared_ptr<Expression>> childs) : ComparatorExpression(op, NonTerminalExpressionType::RANGE, childs), le(DOUBLE_MIN), ge(DOUBLE_MAX), lt(DOUBLE_MIN), gt(DOUBLE_MAX), isRange(true)
{
    setType(ExpressionType::CONSTRAINT);
    // 根据child的操作符类型设置ge, le, gt, lt, isRange
    for (auto&& child : childs)
    {
        ComparatorExpression* chi = static_cast<ComparatorExpression*>(child.get());
        double operand = std::stod(chi->getOperand());
        switch (chi->getNType())
        {
        case NonTerminalExpressionType::EQUAL:
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
        case NonTerminalExpressionType::NOT_EQUAL:
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
        case NonTerminalExpressionType::LESS:
        {
            if (operand < le || operand < lt)
            {
                isRange = false;
            }
            gt = std::min(gt, operand);
        }
        break;
        case NonTerminalExpressionType::LESS_EQUAL:
        {
            if (operand < le || operand < lt)
            {
                isRange = false;
            }
            ge = std::min(ge, operand);
        }
        break;
        case NonTerminalExpressionType::GREATER:
        {
            if (operand > ge || operand > gt)
            {
                isRange = false;
            }
            lt = std::max(lt, operand);
        }
        break;
        case NonTerminalExpressionType::GREATER_EQUAL:
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
    return std::make_shared<ComparatorExpression>(str, ComparatorExpression::hash.at(str)->getNType(), childs);
}

// 判断是否在范围内
bool CompoundComparatorExpression::isInRange(double l, double g)
{
    return isRange && ((l <= le || l <= lt) || (g >= ge || g >= gt));
}

// 获取范围
std::tuple<double, double, bool> CompoundComparatorExpression::getRange()
{
    return std::make_tuple(std::max(le, lt), std::min(ge, gt), lt != gt);
}

//输入表达式hash
std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> InputExpression::hash = {};

void InputExpression::initHash()
{
    auto mtLimit = LIMIT(true, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::ORIGIONAL});
    auto mnLimit = LIMIT(true, EXPRESSIONNUMBERLIMITFLAG, {EXPRESSIONNUMBERLIMITFLAG, ExpressionType::NUMBER});
    auto onLimit = LIMIT(false, EXPRESSIONNUMBERLIMITFLAG, {EXPRESSIONNUMBERLIMITFLAG, ExpressionType::NUMBER});
    auto msLimit = LIMIT(true, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::SOURCE_NAME});

    auto sourceLimits = LIMITLIST(mtLimit);
    auto inputLimits = LIMITLIST(msLimit, mnLimit, onLimit);
    auto labelsLimits = LIMITLIST(msLimit, mnLimit, onLimit);
    auto cellLimits = LIMITLIST(msLimit, mtLimit);
    auto windowLimits = LIMITLIST(msLimit, mnLimit, mnLimit, mnLimit, mnLimit);

    hash.insert({ "SOURCE", PREPTR("SOURCE", ExpressionType::SOURCE_NAME, NonTerminalExpressionType::SOURCE, "source", 1, 1, std::move(sourceLimits)) });
    hash.insert({ "INPUT", PREPTR("INPUT", ExpressionType::ORIGIONAL_LAYER_NAME, NonTerminalExpressionType::INPUT, "input", 2, 3, std::move(inputLimits)) });
    hash.insert({ "LABELS", PREPTR("LABELS", ExpressionType::LABELS_NAME, NonTerminalExpressionType::LABELS, "labels", 2, 3, std::move(labelsLimits)) });
    hash.insert({ "CELL", PREPTR("CELL", ExpressionType::CELL_NAME, NonTerminalExpressionType::CELL, "cell", 2, 2, std::move(cellLimits)) });
    hash.insert({ "WINDOW", PREPTR("WINDOW", ExpressionType::ORIGIONAL_LAYER_NAME, NonTerminalExpressionType::WINDOW, "clip", 5, 5, std::move(windowLimits)) });
}

std::string interpreter::InputExpression::interpret()
{
    std::ostringstream result;
    auto con = hash.find(op)->second->getCondition();
    switch (getNType())
    {
        case NonTerminalExpressionType::SOURCE:
            result << con << "(\"" << children->at(0)->interpret() << "\")";
            break;

        case NonTerminalExpressionType::INPUT:
        case NonTerminalExpressionType::LABELS:
            result << children->at(0)->interpret() << "." << con << "(" << children->at(1)->interpret();
            if (children->size() == 3)
            {
                result << ", " << children->at(2)->interpret();
            }
            result << ")";
            break;
        
        case NonTerminalExpressionType::CELL:
            result << children->at(0)->interpret() << "." << con << "(" << children->at(1)->interpret() << ")";
            break;
        
        case NonTerminalExpressionType::WINDOW:
            result << children->at(0)->interpret() << "." << con << "(p(" << children->at(1)->interpret() << ", " << children->at(2)->interpret() << "), p(" << children->at(3)->interpret() << ", " << children->at(4)->interpret() << "))";
            break;
        default:
            break;
    }
    return result.str();
}

//输出表达式hash
std::unordered_multimap<std::string, std::shared_ptr<PreExpressionInfo>> OutputExpression::hash = {};

//inithash
void OutputExpression::initHash()
{
    auto mlLimit = LIMIT(true, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::LAYER_NAME});
    auto mtLimit = LIMIT(true, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::ORIGIONAL});
    auto otLimit = LIMIT(false, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::ORIGIONAL});
    auto mnLimit = LIMIT(true, EXPRESSIONNUMBERLIMITFLAG, {EXPRESSIONNUMBERLIMITFLAG, ExpressionType::NUMBER});
    auto onLimit = LIMIT(false, EXPRESSIONNUMBERLIMITFLAG, {EXPRESSIONNUMBERLIMITFLAG, ExpressionType::NUMBER});
    auto oTargetLimit = LIMIT(false, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::TARGET_NAME});
    auto oReportLimit = LIMIT(true, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::REPORT_NAME});

    auto outputLimits = LIMITLIST(mlLimit, mnLimit, onLimit, oTargetLimit);
    auto outputLimits0 = LIMITLIST(mlLimit, mtLimit, otLimit, oReportLimit);
    auto targetLimits = LIMITLIST(mtLimit);
    auto newTargetLimits = LIMITLIST(mtLimit);
    auto reportLimits = LIMITLIST(mtLimit, otLimit);
    auto newReportLimits = LIMITLIST(mtLimit, otLimit);

    hash.insert({ "OUTPUT", PREPTR("OUTPUT", ExpressionType::ORIGIONAL, NonTerminalExpressionType::OUTPUT, "output", 2, 4, std::move(outputLimits)) });
    hash.insert({ "OUTPUT", PREPTR("OUTPUT", ExpressionType::ORIGIONAL, NonTerminalExpressionType::OUTPUT, "output", 2, 4, std::move(outputLimits0))});
    hash.insert({ "TARGET", PREPTR("TARGET", ExpressionType::TARGET_NAME, NonTerminalExpressionType::TARGET, "target", 1, 1, std::move(targetLimits)) });
    hash.insert({ "NEWTARGET", PREPTR("NEWTARGET", ExpressionType::TARGET_NAME, NonTerminalExpressionType::NEW_TARGET, "new_target", 1, 1, std::move(newTargetLimits)) });
    hash.insert({ "REPORT", PREPTR("REPORT", ExpressionType::REPORT_NAME, NonTerminalExpressionType::REPORT, "report", 1, 2, std::move(reportLimits)) });
    hash.insert({ "NEWREPORT", PREPTR("NEWREPORT", ExpressionType::REPORT_NAME, NonTerminalExpressionType::NEW_REPORT, "new_report", 1, 2, std::move(newReportLimits)) });
}

std::string interpreter::OutputExpression::interpret()
{
    std::ostringstream result;
    auto con = hash.find(op)->second->getCondition();
    switch (getNType())
    {
        case NonTerminalExpressionType::OUTPUT:
            result << children->at(0)->interpret() << "." << con << "(" << children->at(1)->interpret();
            for (size_t i = 2; i < children->size(); i++)
            {
                result << ", " << children->at(i)->interpret();
            }
            result << ")";
            break;

        case NonTerminalExpressionType::TARGET:
        case NonTerminalExpressionType::NEW_TARGET:
            result << con << "(\"" << children->at(0)->interpret() << "\")";
            break;
        case NonTerminalExpressionType::REPORT:
        case NonTerminalExpressionType::NEW_REPORT:
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

//BY hash
std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> BYExpression::hash = {};

//inithash
void BYExpression::initHash()
{
    auto mnLimit = LIMIT(true, EXPRESSIONNUMBERLIMITFLAG, {EXPRESSIONNUMBERLIMITFLAG, ExpressionType::NUMBER});

    hash.insert({ "BY", PREPTR("BY", ExpressionType::BY_OPTION, NonTerminalExpressionType::BY, "", 1, 1, LIMITLIST(mnLimit))});
}

//hash
std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> GrowOptionExpression::hash = {};

//inithash
void GrowOptionExpression::initHash()
{
    auto mbLimit = LIMIT(true, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::BY_OPTION});

    hash.insert({ "LEFT", PREPTR("LEFT", ExpressionType::GROW_OPTION, NonTerminalExpressionType::LEFT, "", 1, 1, LIMITLIST(mbLimit))});
    hash.insert({ "RIGHT", PREPTR("RIGHT", ExpressionType::GROW_OPTION, NonTerminalExpressionType::RIGHT, "", 1, 1, LIMITLIST(mbLimit))});
    hash.insert({ "TOP", PREPTR("TOP", ExpressionType::GROW_OPTION, NonTerminalExpressionType::TOP, "", 1, 1, LIMITLIST(mbLimit)) });
    hash.insert({ "BOTTOM", PREPTR("BOTTOM", ExpressionType::GROW_OPTION, NonTerminalExpressionType::BOTTOM, "", 1, 1, LIMITLIST(mbLimit)) });
}

//hash
std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> ConvexEdgeOptionExpression::hash = {};

//inithash
void ConvexEdgeOptionExpression::initHash()
{
    auto ol1Limit = LIMIT(false, NEXPRESSIONTYPELIMITFLAG, {NEXPRESSIONTYPELIMITFLAG, NonTerminalExpressionType::LENGTH1});
    auto ol2Limit = LIMIT(false, NEXPRESSIONTYPELIMITFLAG, {NEXPRESSIONTYPELIMITFLAG, NonTerminalExpressionType::LENGTH2});
    auto mlLimit = LIMIT(true, NEXPRESSIONTYPELIMITFLAG, {NEXPRESSIONTYPELIMITFLAG, NonTerminalExpressionType::LENGTH_OPTION});
    auto mcLimit = LIMIT(true, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::CONSTRAINT});

    hash.insert({ "ANGLE1", PREPTR("ANGLE1", ExpressionType::CONVEX_OPTIONS, NonTerminalExpressionType::ANGLE1, "corners(as_edge_pairs)", 1, 2, LIMITLIST(mcLimit, ol1Limit))});
    hash.insert({ "ANGLE2", PREPTR("ANGLE2", ExpressionType::CONVEX_OPTIONS, NonTerminalExpressionType::ANGLE2, "corners(as_edge_pairs)", 1, 2, LIMITLIST(mcLimit, ol2Limit))});
    hash.insert({ "WITH", PREPTR("WITH", ExpressionType::CONVEX_OPTIONS, NonTerminalExpressionType::WITH, "", 1, 1, LIMITLIST(mlLimit))});
    hash.insert({ "EDGE", PREPTR("EDGE", ExpressionType::CONVEX_OPTIONS, NonTerminalExpressionType::EDGE, "", 0, 0, LIMITLIST())});
}

//hash
std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> LengthExpression::hash = {};

//inithash
void LengthExpression::initHash()
{
    auto mcLimit = LIMIT(true, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::CONSTRAINT});

    hash.insert({ "LENGTH1", PREPTR("LENGTH1", ExpressionType::LENGTH_OPTION, NonTerminalExpressionType::LENGTH1, "length", 1, 1, LIMITLIST(mcLimit)) });
    hash.insert({ "LENGTH2", PREPTR("LENGTH2", ExpressionType::LENGTH_OPTION, NonTerminalExpressionType::LENGTH2, "length", 1, 1, LIMITLIST(mcLimit)) });
    hash.insert({ "LENGTH", PREPTR("LENGTH", ExpressionType::LENGTH_OPTION, NonTerminalExpressionType::LENGTH_OPTION, "length", 1, 1, LIMITLIST(mcLimit))});
}

//hash
std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> SizeOptionExpression::hash = {};

//inithash
void SizeOptionExpression::initHash()
{
    hash.insert({ "OVERUNDER", PREPTR("OVERUNDER", ExpressionType::SIZE_OPTION, NonTerminalExpressionType::OVERUNDER, "size", 0, 0, LIMITLIST()) });
    hash.insert({ "UNDEROVER", PREPTR("UNDEROVER", ExpressionType::SIZE_OPTION, NonTerminalExpressionType::UNDEROVER, "size", 0, 0, LIMITLIST()) });
}

//hash
std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> ExtentOptionExpression::hash = {};

//inithash
void ExtentOptionExpression::initHash()
{
    auto mnLimit = LIMIT(true, EXPRESSIONNUMBERLIMITFLAG, {EXPRESSIONNUMBERLIMITFLAG, ExpressionType::NUMBER});

    hash.insert({ "EXTENTS", PREPTR("EXTENTS", ExpressionType::EXTENT, NonTerminalExpressionType::EXTENTS, "ext", 0, 0, LIMITLIST()) });
    hash.insert({ "EXTENDED", PREPTR("EXTENDED", ExpressionType::EXTENT, NonTerminalExpressionType::EXTENDED, "ext", 1, 1, LIMITLIST(mnLimit)) });
}

//hash
std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> RelationsOptionExpression::hash = {};

//inithash
void RelationsOptionExpression::initHash()
{
    auto mcLimit = LIMIT(true, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::CONSTRAINT});
    auto oetLimit = LIMIT(true, NEXPRESSIONTYPELIMITFLAG, {NEXPRESSIONTYPELIMITFLAG, NonTerminalExpressionType::EXTENTS});
    auto oedLimit = LIMIT(false, NEXPRESSIONTYPELIMITFLAG, {NEXPRESSIONTYPELIMITFLAG, NonTerminalExpressionType::EXTENDED});

    hash.insert({ "OPPOSITE", PREPTR("OPPOSITE", ExpressionType::RELATIONS_OPTION, NonTerminalExpressionType::OPPOSITE, "projection", 0, 1, LIMITLIST(oedLimit)) });
    hash.insert({ "SQUARE", PREPTR("SQUARE", ExpressionType::RELATIONS_OPTION, NonTerminalExpressionType::SQUARE, "projection", 0, 0, LIMITLIST()) });
    hash.insert({ "SHILDED", PREPTR("SHILDED", ExpressionType::RELATIONS_OPTION, NonTerminalExpressionType::SHILDED, "shielded", 0, 0, LIMITLIST()) });
    hash.insert({ "PROJecting", PREPTR("PROJecting", ExpressionType::RELATIONS_OPTION, NonTerminalExpressionType::PROJecting, "projecting", 0, 1, LIMITLIST(mcLimit)) });
    hash.insert({ "REGION", PREPTR("REGION", ExpressionType::RELATIONS_OPTION, NonTerminalExpressionType::REGION, "region", 0, 1, LIMITLIST(oetLimit)) });
}

//hash
std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> LeftOptionExpression::hash = {};

//inithash
void LeftOptionExpression::initHash()
{
    auto msLimit = LIMIT(true, NEXPRESSIONTYPELIMITFLAG, {NEXPRESSIONTYPELIMITFLAG, NonTerminalExpressionType::SHILDED});
    auto mpLimit = LIMIT(true, NEXPRESSIONTYPELIMITFLAG, {NEXPRESSIONTYPELIMITFLAG, NonTerminalExpressionType::PROJecting});

    hash.insert({ "NOT", PREPTR("NOT", ExpressionType::LEFT_OPTION, NonTerminalExpressionType::NOT, "PROJecting", 1, 1, LIMITLIST(mpLimit))});
    hash.insert({ "EXCLUDE", PREPTR("EXCLUDE", ExpressionType::LEFT_OPTION, NonTerminalExpressionType::EXCLUDE, "SHILDED", 1, 1, LIMITLIST(msLimit))});
}

// LogicalExpression
LogicalExpression::LogicalExpression(std::string op, NonTerminalExpressionType d, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, d, childs) {
    setType(ExpressionType::LAYER_NAME);
}

std::string LogicalExpression::interpret()
{
    std::ostringstream result;
    switch (getNType())
    {
    case NonTerminalExpressionType::AND:
        LogicSelfinterpreter(result);
        break;
    case NonTerminalExpressionType::AND_SELF:
        ANDSelfInterpreter(result);
        break;
    case NonTerminalExpressionType::OR:
        LogicSelfinterpreter(result);
        break;
    case NonTerminalExpressionType::XOR:
        LogicSelfinterpreter(result);
        break;
    case NonTerminalExpressionType::NOT:
        result << NonterminalExpression::interpret();
        break;
    case NonTerminalExpressionType::ENCLOSE:
    case NonTerminalExpressionType::INTERACT:
    {
        setOption(2, false, true);
        result << NonterminalExpression::interpret();
    }
    break;
    case NonTerminalExpressionType::LENGTH:
    {
        setOption(1);
        result << NonterminalExpression::interpret();
    }
    break;
    case NonTerminalExpressionType::GROW:
        GrowInterpreter(result);
        break;
    case NonTerminalExpressionType::SIZE:
        result << NonterminalExpression::interpret();
        break;
    case NonTerminalExpressionType::ANGLE:
    {
        setOption(1, true);
        result << NonterminalExpression::interpret();
    }
    break;
    case NonTerminalExpressionType::CONVEX:
        ConvexInterpreter(result);
        break;
    case NonTerminalExpressionType::CONVEX_DETAIL:
        ConvexDetailInterpreter(result);
        break;
    case NonTerminalExpressionType::ENCLOSURE:
    case NonTerminalExpressionType::INTERNAL:
    case NonTerminalExpressionType::EXTERNAL:
        RelationsInterpreter(result, 1);
        break;
    case NonTerminalExpressionType::INTERNAL_SELF:
    case NonTerminalExpressionType::EXTERNAL_SELF:
        RelationsInterpreter(result, 0);
        break;

    default:
        break;
    }
    return result.str();
}

void LogicalExpression::LogicSelfinterpreter(std::ostringstream &result)
{
    if (children->size() == 1)
    {
        result << op << "_SELF(" << children->at(0)->interpret() << ")";
    }
    else
    {
        result << NonterminalExpression::interpret();
    }
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
        switch (chi->getNType())
        {
        case NonTerminalExpressionType::OPPOSITE:
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
        case NonTerminalExpressionType::SQUARE:
        {
            strCon << (isDoubleMode ? ", " : "") << chi->interpret();
            isDoubleMode = true;
            ext = std::stod(comp->getOperand());
        }
        break;
        case NonTerminalExpressionType::SHILDED:
        {
            strCon << (isDoubleMode ? ", " : "") << chi->interpret();
            isDoubleMode = true;
        }
        break;
        case NonTerminalExpressionType::PROJecting:
        {
            if (chi->interpret() != "NOT")
            {
                strCon << (isDoubleMode ? ", " : "") << chi->interpret();
                isDoubleMode = true;
            }
        }
        break;
        case NonTerminalExpressionType::REGION:
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
        int id = static_cast<int>(chi->getNType()) - static_cast<int>(NonTerminalExpressionType::LEFT);
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

std::string LogicalExpression::GetCondition() 
{
    for (auto it = hash.find(op); it != hash.end(); it++)
    {
        if (it->second->getNType() == getNType())
        {
            return it->second->getCondition();
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
        auto [l, g, r] = chi->getRange();
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

std::unordered_multimap<std::string, std::shared_ptr<PreExpressionInfo>> LogicalExpression::hash = {};

//inithash
void LogicalExpression::initHash()
{
    auto val = std::vector<NonTerminalExpressionType>{NonTerminalExpressionType::OPPOSITE, NonTerminalExpressionType::SQUARE};
    auto mlLimit = LIMIT(true, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::LAYER_NAME});
    auto olLimit = LIMIT(false, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::LAYER_NAME});
    auto mcLimit = LIMIT(true, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::CONSTRAINT});
    auto ocLimit = LIMIT(false, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::CONSTRAINT});
    auto mgLimit = LIMIT(true, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::GROW_OPTION});
    auto ogLimit = LIMIT(false, EXPRESSIONTYPELIMITFLAG | EXPRESSIONCOUNTLIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::GROW_OPTION}, {EXPRESSIONCOUNTLIMITFLAG, 1});
    auto mbLimit = LIMIT(true, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::BY_OPTION});
    auto osLimit = LIMIT(false, EXPRESSIONTYPELIMITFLAG, {EXPRESSIONTYPELIMITFLAG, ExpressionType::SIZE_OPTION});
    auto mEdgeLimit = LIMIT(true, NEXPRESSIONTYPELIMITFLAG, {NEXPRESSIONTYPELIMITFLAG, NonTerminalExpressionType::EDGE});
    auto mAngle1Limit = LIMIT(true, NEXPRESSIONTYPELIMITFLAG, {NEXPRESSIONTYPELIMITFLAG, NonTerminalExpressionType::ANGLE1});
    auto mAngle2Limit = LIMIT(true, NEXPRESSIONTYPELIMITFLAG, {NEXPRESSIONTYPELIMITFLAG, NonTerminalExpressionType::ANGLE2});
    auto oWithLimit = LIMIT(false, NEXPRESSIONTYPELIMITFLAG, {NEXPRESSIONTYPELIMITFLAG, NonTerminalExpressionType::WITH});
    auto oRelLimit = LIMIT(false, EXPRESSIONTYPELIMITFLAG | EXPRESSIONCOUNTLIMITFLAG | NEXPRESSIONONELIMITINNFLAG, 
        {EXPRESSIONTYPELIMITFLAG, ExpressionType::RELATIONS_OPTION}, {EXPRESSIONCOUNTLIMITFLAG, 1}, {NEXPRESSIONONELIMITINNFLAG, std::move(val)});

    auto andselfLimit = LIMITLIST(mlLimit, mcLimit);
    auto andLimit = LIMITLIST(mlLimit, olLimit);
    auto orLimit = LIMITLIST(mlLimit, olLimit);
    auto xorLimit = LIMITLIST(mlLimit, olLimit);
    auto notLimit = LIMITLIST(mlLimit, mlLimit);
    auto encloseLimit = LIMITLIST(mlLimit, mlLimit, ocLimit);
    auto interactLimit = LIMITLIST(mlLimit, mlLimit, ocLimit);
    auto lengthLimit = LIMITLIST(mlLimit, ocLimit);
    auto growLimit = LIMITLIST(mlLimit, mgLimit, ogLimit, ogLimit, ogLimit);
    auto sizeLimit = LIMITLIST(mlLimit, mbLimit, osLimit);
    auto angleLimit = LIMITLIST(mlLimit, mcLimit);
    auto convexDetailLimit = LIMITLIST(mEdgeLimit, mlLimit, mAngle1Limit, mAngle2Limit, oWithLimit);
    auto convexLimit = LIMITLIST(mEdgeLimit, mlLimit, mcLimit, oWithLimit);
    auto enclosureLimit = LIMITLIST(mlLimit, mlLimit, mcLimit, oRelLimit, oRelLimit, oRelLimit, oRelLimit);
    auto internalLimit = LIMITLIST(mlLimit, mlLimit, mcLimit, oRelLimit, oRelLimit, oRelLimit, oRelLimit);
    auto externalLimit = LIMITLIST(mlLimit, mlLimit, mcLimit, oRelLimit, oRelLimit, oRelLimit, oRelLimit);
    auto internalLimit0 = LIMITLIST(mlLimit, mcLimit, oRelLimit, oRelLimit, oRelLimit, oRelLimit);
    auto externalLimit0 = LIMITLIST(mlLimit, mcLimit, oRelLimit, oRelLimit, oRelLimit, oRelLimit);

    hash.insert({"AND", PREPTR("AND_SELF", ExpressionType::LAYER_NAME, NonTerminalExpressionType::AND_SELF, "AND_SELF", 2, 2, std::move(andselfLimit))});
    hash.insert({"AND", PREPTR("AND", ExpressionType::LAYER_NAME, NonTerminalExpressionType::AND, "AND", 1, 2, std::move(andLimit))});
    hash.insert({"OR", PREPTR("OR", ExpressionType::LAYER_NAME, NonTerminalExpressionType::OR, "OR", 1, 2, std::move(orLimit))});
    hash.insert({"XOR", PREPTR("XOR", ExpressionType::LAYER_NAME, NonTerminalExpressionType::XOR, "XOR", 1, 2, std::move(xorLimit))});
    hash.insert({"NOT", PREPTR("NOT", ExpressionType::LAYER_NAME, NonTerminalExpressionType::NOT, "NOT", 2, 2, std::move(notLimit))});
    hash.insert({"ENCLOSE", PREPTR("ENCLOSE", ExpressionType::LAYER_NAME, NonTerminalExpressionType::ENCLOSE, "covering", 2, 3, std::move(encloseLimit))});
    hash.insert({"INTERACT", PREPTR("INTERACT", ExpressionType::LAYER_NAME, NonTerminalExpressionType::INTERACT, "interacting", 2, 3, std::move(interactLimit))});
    hash.insert({"LENGTH", PREPTR("LENGTH", ExpressionType::LAYER_NAME, NonTerminalExpressionType::LENGTH, "length", 2, 2, std::move(lengthLimit))});
    hash.insert({"GROW", PREPTR("GROW", ExpressionType::LAYER_NAME, NonTerminalExpressionType::GROW, "GROW", 2, 5, std::move(growLimit))});
    hash.insert({"SIZE", PREPTR("SIZE", ExpressionType::LAYER_NAME, NonTerminalExpressionType::SIZE, "SIZE", 2, 3, std::move(sizeLimit))});
    hash.insert({"ANGLE", PREPTR("ANGLE", ExpressionType::LAYER_NAME, NonTerminalExpressionType::ANGLE, "angle", 2, 2, std::move(angleLimit))});
    hash.insert({"CONVEX", PREPTR("CONVEXEDGE_DETAIL", ExpressionType::LAYER_NAME, NonTerminalExpressionType::CONVEX_DETAIL, "CONVEX", 4, 5, std::move(convexDetailLimit))});
    hash.insert({"CONVEX", PREPTR("CONVEXEDGE", ExpressionType::LAYER_NAME, NonTerminalExpressionType::CONVEX, "CONVEX", 3, 4, std::move(convexLimit))});
    hash.insert({"ENCLOSURE", PREPTR("ENCLOSURE", ExpressionType::LAYER_NAME, NonTerminalExpressionType::ENCLOSURE, "enclosed", 3, 7, std::move(enclosureLimit))});
    hash.insert({"INTERNAL", PREPTR("INTERNAL", ExpressionType::LAYER_NAME, NonTerminalExpressionType::INTERNAL, "overlap", 3, 7, std::move(internalLimit))});
    hash.insert({"EXTERNAL", PREPTR("EXTERNAL", ExpressionType::LAYER_NAME, NonTerminalExpressionType::EXTERNAL, "separation", 3, 7, std::move(externalLimit))});
    hash.insert({"INTERNAL", PREPTR("INTERNAL_SELF", ExpressionType::LAYER_NAME, NonTerminalExpressionType::INTERNAL_SELF, "width", 2, 6, std::move(internalLimit0))});
    hash.insert({"EXTERNAL", PREPTR("EXTERNAL_SELF", ExpressionType::LAYER_NAME, NonTerminalExpressionType::EXTERNAL_SELF, "space", 2, 6, std::move(externalLimit0))});
}

std::string ConvexEdgeOptionExpression::interpret()
{
    std::ostringstream result;
   switch (getNType())
   {
        case NonTerminalExpressionType::ANGLE1:
        case NonTerminalExpressionType::ANGLE2:
            {
                ComparatorExpression* chi = static_cast<ComparatorExpression*>(children->at(0).get());
                chi->setReverse();
                chi->setOption(hash.at(op)->getCondition());
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
            break;
        case NonTerminalExpressionType::WITH:
            result << children->at(0)->interpret();
            break;
        case NonTerminalExpressionType::EDGE:
            result << op;
            break;
        
        default:
            break;
   }

    return result.str();
}

bool LeftOptionExpression::parser(const std::vector<std::string>& tokens, std::stack<std::shared_ptr<Expression>>& stack, int& i, const PreExpressionInfo& path)
{
    if (stack.size() >= 1)
    {
        NonterminalExpression* exp = dynamic_cast<NonterminalExpression*>(stack.top().get());
        if (exp && exp->getOp() == path.getCondition())
        {
            std::vector<std::shared_ptr<Expression>> childs;
            exp->getChildren()->push_back(std::make_shared<LeftOptionExpression>(tokens[i], path.getNType(), childs));
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

RelationsOptionExpression::RelationsOptionExpression(std::string str, NonTerminalExpressionType id, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(str, id, childs) {
    setType(ExpressionType::RELATIONS_OPTION);
}

std::string RelationsOptionExpression::interpret()
{
    std::ostringstream result;
    switch (getNType())
    {
    case NonTerminalExpressionType::PROJecting:
    {
        if (children->size() == 0)
        {
            result << hash.at(op)->getCondition() << ">0";
        }
        else if (Commmon::isInstance<LeftOptionExpression>(children->at(0).get()))
        {
            result << children->at(0)->interpret() << op;
        }
        else if (Commmon::isInstance<ComparatorExpression>(children->at(0).get()))
        {
            ComparatorExpression* chi = static_cast<ComparatorExpression*>(children->at(0).get());
            chi->setOption(hash.at(op)->getCondition());
            result << chi->interpret();
        }
        else
        {
            throw std::runtime_error("The expression is not supported!");
        }
    }
    break;
    case NonTerminalExpressionType::REGION:
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
        result << hash.at(op)->getCondition();
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

std::shared_ptr<Expression> Parser::parse(std::string& str, Context* context)
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
    return parse(tokenize(str), context);
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
std::shared_ptr<Expression> Parser::parse(const std::vector<std::string>& tokens, Context* context)
{
    bool bOK = true;
    std::stack<std::shared_ptr<Expression>> stack;
    std::stack<std::shared_ptr<Expression>> fixStack;
    for (int i = tokens.size() - 1; i >= 0; i--)
    {
        bool isParsed = false;
        if (FixExpression::hash.find(tokens[i]) != FixExpression::hash.end())
        {
            fixStack.push(std::make_shared<FixExpression>(tokens[i]));
            isParsed = true;
        }
        
        auto range = getIter(tokens[i]);
        for (auto it = range.first; it != range.second && !isParsed; it++)
        {
            isParsed |= it->second.first(tokens, stack, i, *it->second.second);
        }

        if (!isParsed)
        {
            auto exp =  std::make_shared<TerminalExpression>(tokens[i]);
            exp->setType(context->getTypeByContext(tokens[i]));
            stack.push(exp);
        }
    }

    while ((fixStack.size() > 0) && (stack.size() - fixStack.size() == 1))
    {
        FixExpression* fix = static_cast<FixExpression*>(fixStack.top().get());
        fixStack.pop();
        auto right = stack.top();
        stack.pop();
        auto left = stack.top();
        stack.pop();
        bOK |= fix->setRight(right);
        bOK |= fix->setLeft(left);
        if (bOK)
        {
            context->addContext(left->getType(), left->interpret());
        }
        else
        {
            throw std::runtime_error(fmt::format("The expressiontype between {} is not supported!", fix->getOp()));
        }
        
        stack.push(std::shared_ptr<Expression>(fix));
    }

    if (stack.size() != 1)
    {
        throw std::runtime_error("The expression is not supported!");
    }
    return stack.top();
}

template <typename T>
bool Parser::parser(const std::vector<std::string>& tokens, std::stack<std::shared_ptr<Expression>>& stack, int& i, PreExpressionInfo& path)
{
    std::vector<std::shared_ptr<Expression>> childs;
    int min = path.getMinOptions();
    int max = path.getMaxOptions();
    auto& limits = path.getLimits();
    int j = 0;

    if (stack.size() < min)
    {
        return false;
    }

    for (; j < min; j++)
    {
        if (limits[j]->isMatch(stack.top().get(), childs))
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

    while (j < max && stack.size() > 0 && limits[j]->isMatch(stack.top().get(), childs))
    {
        childs.push_back(stack.top());
        stack.pop();
        j++;
    }

    stack.push(std::make_shared<T>(tokens[i], path.getNType(), childs));
    return true;
}

template <typename T>
void Parser::Register2Parser()
{
    T::initHash();
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
            result.push_back(Parser::parse(lines[i], context.get()));
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
    context = std::make_unique<Context>();
    context->addContext(ExpressionType::ORIGIONAL, "");
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
            expressions.push_back(Parser::parse(lines[i], context.get()));
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
    FixExpression::initHash();
    ExpressionOptionLimit::init();

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

