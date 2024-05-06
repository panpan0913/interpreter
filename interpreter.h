#ifndef INTERANDPARSER_H
#define INTERANDPARSER_H

// Include any necessary libraries or headers
#include "ExpressionEnum.h"
#include <string>
#include <memory>
#include <vector>
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <stack>
#include <sstream>
#include <algorithm>
#include <limits>
#include <iomanip>
#include <functional>
#include <typeinfo>
#include <regex>
#include <fstream>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <spdlog/sinks/rotating_file_sink.h>
#include <any>

// Declare any global variables or constants
namespace interpreter {
    class Expression {
    private:
        ExpressionType type;
    public:
        Expression() = default;
        Expression(int type) : type(static_cast<ExpressionType>(type)) {}
        Expression(const Expression&) = default;
        Expression& operator=(const Expression&) = default;
        Expression(ExpressionType type) : type(type) {}
        virtual ~Expression() = default;
        virtual std::string interpret() = 0;
        virtual ExpressionType getType() const {
            return type;
        }
        virtual void setType(ExpressionType id) {
            this->type = id;
        }
    };

    using isMatchFunc = std::function<bool(Expression*, const std::vector<std::shared_ptr<Expression>>&, std::any val)>;

    class ExpressionOptionLimit {
    private:
        bool isNessary;
        std::unordered_map<ExpressionLimitType, std::any> limitMap;
        static std::unordered_map<ExpressionLimitType, isMatchFunc> isMatchFuncs;
    public:
        ExpressionOptionLimit(bool isNessary, std::unordered_map<ExpressionLimitType, std::any>&& map) : isNessary(isNessary), limitMap(map) {}
        bool getIsNessary() const {
            return isNessary;
        }
        std::unordered_map<ExpressionLimitType, std::any>& getLimitValue() {
            return limitMap;
        }
        static void init();
        bool isMatch(Expression* exp, const std::vector<std::shared_ptr<Expression>>& childs);
        static bool isExpTypeMatch(Expression* exp, const std::vector<std::shared_ptr<Expression>>&, std::any val);
        static bool isNExpTypeMatch(Expression* exp, const std::vector<std::shared_ptr<Expression>>&, std::any val);
        static bool isExpCountMatch(Expression* exp, const std::vector<std::shared_ptr<Expression>>& childs, std::any val);
        static bool isNumberMatch(Expression* exp, const std::vector<std::shared_ptr<Expression>>&, std::any val);
        static bool isNExp1InNMatch(Expression* exp, const std::vector<std::shared_ptr<Expression>>& childs, std::any val);
    };

    class PreExpressionInfo {
    private:
        std::string name;
        ExpressionType type;
        NonTerminalExpressionType nType;
        std::string condition;
        std::weak_ptr<PreExpressionInfo> reverse;//
        int minOptions;
        int maxOptions;
        std::shared_ptr<std::vector<std::shared_ptr<ExpressionOptionLimit>>> limits;
    public:
        PreExpressionInfo() = default;
        PreExpressionInfo(const PreExpressionInfo&) = default;
        PreExpressionInfo& operator=(const PreExpressionInfo&) = default;
        ~PreExpressionInfo() = default;
        PreExpressionInfo(std::string name, ExpressionType type, NonTerminalExpressionType nType, std::string condition, int minOptions, int maxOptions, std::shared_ptr<std::vector<std::shared_ptr<ExpressionOptionLimit>>>&& limits) : name(name), type(type), nType(nType), condition(condition), minOptions(minOptions), maxOptions(maxOptions), limits(limits){}
        std::string getName() const {
            return name;
        }
        ExpressionType getType() const {
            return type;
        }
        NonTerminalExpressionType getNType() const {
            return nType;
        }
        std::string getCondition() const {
            return condition;
        }
        int getMinOptions() const {
            return minOptions;
        }
        int getMaxOptions() const {
            return maxOptions;
        }
        std::vector<std::shared_ptr<ExpressionOptionLimit>>& getLimits() {
            return *limits;
        }

        void setReverse(std::shared_ptr<PreExpressionInfo> reverse) {
            this->reverse = reverse;
        }

        std::shared_ptr<PreExpressionInfo> getReverse() {
            return reverse.lock();
        }
    };

    class Commmon {
    public:
        static std::string toUppercase(const std::string& str)
        {
            std::string res = str;
            std::transform(res.begin(), res.end(), res.begin(), ::toupper);
            return res;
        }

        template <typename T>
        static std::string name() {
            return typeid(T).name();
        }

        template <typename T>
        static bool isInstance(Expression* exp) {
            return dynamic_cast<T*>(exp) != nullptr;
        }
    };

    class TerminalExpression : public Expression {
    protected:
        std::string str;
    public:
        TerminalExpression(const std::string& str) : Expression(ExpressionType::ORIGIONAL), str(str) {}
        std::string interpret() override { return str; }
    };

    class NonterminalExpression : public Expression {
    protected:
        std::string op;
        NonTerminalExpressionType nType;
        std::shared_ptr<std::vector<std::shared_ptr<Expression>>> children;
    public:
        NonterminalExpression(std::string op, NonTerminalExpressionType id, std::vector<std::shared_ptr<Expression>> childs);
        virtual std::string interpret() override;

        std::string getOp() const {
            return op;
        }
        //get children
        std::shared_ptr<std::vector<std::shared_ptr<Expression>>> getChildren() {
            return children;
        }

        NonTerminalExpressionType getNType() const {
            return nType;
        }
    };

    //Fix expression
    class FixExpression : public Expression {
    private:
        std::string op;
        std::shared_ptr<Expression> left;
        std::shared_ptr<Expression> right;
    public:
        FixExpression() = default;
        ~FixExpression() = default;
        std::string interpret() override{
            return fmt::format("{} {} {}", left->interpret(), op, right->interpret());
        }
        FixExpression(std::string op) : Expression(hash.at(op)), op(op) {}
        FixExpression(std::string op, std::shared_ptr<Expression> left, std::shared_ptr<Expression> right) : op(op), left(left), right(right) {}

        bool setRight(std::shared_ptr<Expression> ri) {
            right = ri;
            return true;
        }

        bool setLeft(std::shared_ptr<Expression> le) {
            left = le;
            if (left->getType() == ExpressionType::ORIGIONAL)
            {
                left->setType(right->getType());
            }
            else if (left->getType() != right->getType())
            {
                return false;
            }
            
            return true;
        }

        std::shared_ptr<Expression> getLeft() {
            return left;
        }

        std::shared_ptr<Expression> getRight() {
            return right;
        }

        std::string getOp() {
            return op;
        }

        static std::unordered_map<std::string, ExpressionType> hash;
        static void initHash();
    };



    class ComparatorExpression : public NonterminalExpression {
    private:
        bool isDoubleDirection;
        bool isReverse = false;
        std::string option;
    public:
        static std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> hash;
        static void initHash();

        ComparatorExpression(std::string op, NonTerminalExpressionType id, std::vector<std::shared_ptr<Expression>> childs);
        std::string interpret() override;

        static bool isComparator(const std::string& str);

        static bool containsComparator(const std::string& str);

        static std::string getComparator(const std::string& str);

        static std::string getOperand(const std::string& str);

        void setOption(std::string option);
        std::string getOption();

        std::string getComparator();
        std::string getReverseComparator() const;

        int getComparatorValue() const;

        std::string getOperand();

        void setDoubleDirection(bool isDoubleDirection);

        void setReverse();

        bool getReverse();

        bool getDoubleDirection() const;
        virtual bool isInRange(double l, double g);
        virtual std::tuple<double, double, bool> getRange();
    };

    class CompoundComparatorExpression : public ComparatorExpression {
    private:
        double le;
        double ge;
        double lt;
        double gt;
        bool isRange;
    public:
        CompoundComparatorExpression(std::string op, std::vector<std::shared_ptr<Expression>> childs);

        std::string interpret() override;

        static bool isThis(const std::string& str);

        static bool parser(const std::vector <std::string>& tokens, std::stack<std::shared_ptr<Expression>>& stack, int& i);

        static std::shared_ptr<Expression> ParserComp(const std::shared_ptr<Expression>& exp, const std::string& str);
        bool isInRange(double l, double g) override;
        std::tuple<double, double, bool> getRange() override;
    };

    //输入表达式
    class InputExpression : public NonterminalExpression {
    public:
        InputExpression(std::string op, NonTerminalExpressionType id, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, id, childs) {
            setType(hash.at(op)->getType());
        }
        std::string interpret() override;
        static std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> hash;
        static void initHash();
    };

    //输出输入表达式
    class OutputExpression : public NonterminalExpression {
    public:
        OutputExpression(std::string op, NonTerminalExpressionType id, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, id, childs) {
            setType(hash.find(op)->second->getType());
        }
        std::string interpret() override ;
        static std::unordered_multimap<std::string, std::shared_ptr<PreExpressionInfo>> hash;
        static void initHash();
    };

    //
    class BYExpression : public NonterminalExpression {
    public:
        BYExpression(std::string op, NonTerminalExpressionType id, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, id, childs) {
            setType(ExpressionType::BY_OPTION);
        }
        std::string interpret() override {
            std::ostringstream result;
            double value = std::stod(children->at(0)->interpret());
            result << std::fixed << std::setprecision(4) << value;
            return result.str();
        }
        static std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> hash;
        static void initHash();
    };

    class GrowOptionExpression : public NonterminalExpression {
    public:
        GrowOptionExpression(std::string op, NonTerminalExpressionType id, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, id, childs) {
            setType(ExpressionType::GROW_OPTION);
        }

        std::string interpret() override {
            return children->at(0)->interpret();
        }

        static std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> hash;
        static void initHash();
    };

    class SizeOptionExpression : public NonterminalExpression {
    public:
        SizeOptionExpression(std::string op, NonTerminalExpressionType d, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, d, childs) {
            setType(ExpressionType::SIZE_OPTION);
        }

        std::string interpret() override {
            return hash.at(op)->getCondition();
        }

        static std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> hash;
        static void initHash();
    };

    class LengthExpression : public NonterminalExpression {
    public:
        LengthExpression(std::string op, NonTerminalExpressionType d, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, d, childs) {
            setType(ExpressionType::LENGTH_OPTION);
        }

        std::string interpret() override {
            ComparatorExpression* comp = dynamic_cast<ComparatorExpression*>(children->at(0).get());
            comp->setOption(hash.at(op)->getCondition());
            return children->at(0)->interpret();
        }

        //hash
        static std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> hash;
        static void initHash();
    };

    class ConvexEdgeOptionExpression : public NonterminalExpression {
    public:
        ConvexEdgeOptionExpression(std::string op, NonTerminalExpressionType d, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, d, childs) {
            setType(ExpressionType::CONVEX_OPTIONS);
        }

        std::string interpret() override;

        static std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> hash;
        static void initHash();
    };

    class ExtentOptionExpression : public NonterminalExpression {
    public:
        ExtentOptionExpression(std::string op, NonTerminalExpressionType d, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, d, childs) {
            setType(ExpressionType::EXTENT);
        }

        std::string interpret() override {
            return (getNType() == NonTerminalExpressionType::EXTENTS) ? "0.1" : children->at(0)->interpret();
        }

        static std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> hash;
        static void initHash();
    };

    class RelationsOptionExpression : public NonterminalExpression {
    public:
        RelationsOptionExpression(std::string str, NonTerminalExpressionType id, std::vector<std::shared_ptr<Expression>> childs);

        std::string interpret() override;

        static std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> hash;
        static void initHash();
    };

    class LeftOptionExpression : public NonterminalExpression {
    public:
        LeftOptionExpression(std::string op, NonTerminalExpressionType d, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, d, childs) {
            setType(ExpressionType::LEFT_OPTION);
        }

        std::string interpret() override {
            return op;
        }

        static bool parser(const std::vector<std::string>& tokens, std::stack<std::shared_ptr<Expression>>& stack, int& i, const PreExpressionInfo& path);

        static std::unordered_map<std::string, std::shared_ptr<PreExpressionInfo>> hash;
        static void initHash();
    };

    class LogicalExpression : public NonterminalExpression {
    public:
        LogicalExpression(std::string op, NonTerminalExpressionType d, std::vector<std::shared_ptr<Expression>> childs);

        std::string interpret() override;

        void LogicSelfinterpreter(std::ostringstream &result);

        void RelationsInterpreter(std::ostringstream& result, int mode);

        void ConvexDetailInterpreter(std::ostringstream& result);

        void ConvexInterpreter(std::ostringstream& result);

        void GrowInterpreter(std::ostringstream& result);

        void setOption(int c, bool r = false, bool d = false);

        //getCondition
        std::string GetCondition();

        void ANDSelfInterpreter(std::ostringstream& result);

        static std::unordered_multimap<std::string, std::shared_ptr<PreExpressionInfo>> hash;
        static void initHash();
    };

        //context class
    class Context {
    private:
        std::unordered_map<ExpressionType, std::shared_ptr<std::vector<std::string>>> ContextMap = {};
    public:
        Context() = default;
        ~Context() = default;
        void addContext(ExpressionType type, std::string&& context){
            if (ContextMap.find(type) == ContextMap.end())
            {
                ContextMap[type] = std::make_shared<std::vector<std::string>>();
            }
            ContextMap[type]->push_back(context);
        }

        std::vector<std::string>& getContext(ExpressionType type){
            return *ContextMap[type];
        }

        bool isInContext(ExpressionType type, std::string& str)
        {
            if (ContextMap.find(type) == ContextMap.end())
            {
                return false;
            }
            return std::find(ContextMap[type]->begin(), ContextMap[type]->end(), str) != ContextMap[type]->end();
        }

        auto getTypeByContext(const std::string& str)
        {
            if (ContextMap.empty())
            {
                return ExpressionType::ORIGIONAL;
            }
            
            for (auto& [key, value] : ContextMap)
            {
                if (std::find(value->begin(), value->end(), str) != value->end())
                {
                    return key;
                }
            }
            return ExpressionType::ORIGIONAL;
        }
    };


    using ParserFunc = std::function<bool(const std::vector<std::string>&, std::stack<std::shared_ptr<Expression>>&, int&, PreExpressionInfo&)>;

    class Parser {
    private:
        static std::unordered_multimap<std::string, std::pair<ParserFunc, std::shared_ptr<PreExpressionInfo>>> parserFuncMap;
    public:
        static auto getIter(std::string str);

        template <typename T>
        static bool parser(const std::vector<std::string>& tokens, std::stack<std::shared_ptr<Expression>>& stack, int& i, PreExpressionInfo& path);

        static std::vector<std::string> tokenize(const std::string& str);

        static std::shared_ptr<Expression> parse(std::string& str, Context* cont);

        static std::shared_ptr<Expression> parse(const std::vector<std::string>& tokens, Context* cont);

        template<typename T>
        static void addCorrectExpressionMap();

        template <typename T>
        static void Register2Parser();
    };


    using SetRuningState = std::function<void(int)>;

    class InterPreterSingle {
    private:
        bool isSuccessful = false;
        std::string path;
        std::vector<std::string> lines = {};
        std::vector<std::shared_ptr<Expression>> expressions = {};
        std::thread thread;
        std::string errStr = "";
        std::mutex mtx;
        std::condition_variable cv;
        bool paused = false;
        bool stopped = false;
        int breakLine = -1;
        bool isBreak = false;
        int runTimes = 0;
        std::unique_ptr<Context> context = std::make_unique<Context>();
    public:
        SetRuningState setRuningState = nullptr;
        InterPreterSingle(const std::string& path);
        ~InterPreterSingle();

        void run();

        std::string read();

        void write();

        std::vector<std::string> readLines();

        std::vector<std::string> split(const std::string& str);

        std::string join(const std::vector<std::string>& strs);

        auto parseLines();

        std::string Start();

        void Pause();

        void stop();

        void resume();

        void SetBreakPoint(int line);

        bool getIsBreak() {
            std::unique_lock<std::mutex> lck(mtx);
            return isBreak;
        }

        std::string getErrStr() {
            std::unique_lock<std::mutex> lck(mtx);
            return errStr;
        }

        int getRunTimes() {
            std::unique_lock<std::mutex> lck(mtx);
            return runTimes;
        }

        bool getPaused() {
            std::unique_lock<std::mutex> lck(mtx);
            return paused;
        }

        int getBreakLine() {
            std::unique_lock<std::mutex> lck(mtx);
            return breakLine;
        }
    };

    //interpreter Map��
    class Interpreter {
    private:
        static std::unordered_map<std::string, std::shared_ptr<InterPreterSingle>> interpreterMap;
        static bool isParserInit;
    public:
        static void ParserInit();
        static void Init();
        static std::string addInterpreter(const std::string& path, SetRuningState set = nullptr);

        static std::shared_ptr<InterPreterSingle> getInterpreter(const std::string& path);

        static void removeInterpreter(const std::string& path);

        static std::string Run(const std::string& path, SetRuningState set = nullptr);

        static void Pause(const std::string& path);

        static void Resume(const std::string& path);

        static void Stop(const std::string& path);

        static void SetBreakPoint(const std::string& path, int line);

        static bool getIsBreak(const std::string& path);

        static int getRunTimes(const std::string& path);

        static bool getPaused(const std::string& path);

        static int getBreakLine(const std::string& path);
    };
}

#endif // INTERANDPARSER_H