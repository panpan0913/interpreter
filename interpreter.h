#ifndef INTERANDPARSER_H
#define INTERANDPARSER_H

// Include any necessary libraries or headers
#include <string>
#include <memory>
#include <vector>
#include <iostream>
#include <unordered_map>
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



// Declare any global variables or constants
namespace interpreter {
    class Expression {
    public:
        virtual std::string interpret() = 0;
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
        TerminalExpression(const std::string& str) : str(str) {}
        std::string interpret() override { return str; }
    };

    //special expression
    class TypicalExpression : public TerminalExpression {
    private:
        std::string type;
    public:
        TypicalExpression(const std::string& str, std::string&& type) : TerminalExpression(str), type(type) {}
        std::string getType() const { return type; }
    };

    class NumberExpression : public TypicalExpression {
    private:
        double value;
    public:
        NumberExpression(const double val) : TypicalExpression(fmt::format("{:.4}", val), "NUMBER"), value(val) {}
        double getValue() const { return value; }   
    };

    class NonterminalExpression : public Expression {
    protected:
        int id;
        std::string op;
        std::shared_ptr<std::vector<std::shared_ptr<Expression>>> children;
    public:
        struct ExpressionPath {
            int id;
            std::string condition;
            int minOptions;
            int maxOptions;
            std::vector<std::string> optionTypes;
        };

        NonterminalExpression(std::string op, int id, std::vector<std::shared_ptr<Expression>> childs);
        virtual std::string interpret() override;
        int getId() const {
            return id;
        }
        std::string getOp() const {
            return op;
        }
        //get children
        std::shared_ptr<std::vector<std::shared_ptr<Expression>>> getChildren() {
            return children;
        }
    };

    class ComparatorExpression : public NonterminalExpression {
    private:
        bool isDoubleDirection;
        bool isReverse = false;
        std::string option;
    public:
        static const std::unordered_map<std::string, ExpressionPath> hash;

        ComparatorExpression(std::string op, int id, std::vector<std::shared_ptr<Expression>> childs);
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
        InputExpression(std::string op, int id, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, id, childs) {}
        std::string interpret() override;
        static const std::unordered_map<std::string, ExpressionPath> hash;
    };

    //输出输入表达式
    class OutputExpression : public NonterminalExpression {
    public:
        OutputExpression(std::string op, int id, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, id, childs) {}
        std::string interpret() override ;
        static const std::unordered_map<std::string, ExpressionPath> hash;
    };

    //
    class BYExpression : public NonterminalExpression {
    public:
        BYExpression(std::string op, int id, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, id, childs) {}
        std::string interpret() override {
            std::ostringstream result;
            double value = std::stod(children->at(0)->interpret());
            result << std::fixed << std::setprecision(4) << value;
            return result.str();
        }
        static const std::unordered_map<std::string, ExpressionPath> hash;
    };

    class GrowOptionExpression : public NonterminalExpression {
    public:
        GrowOptionExpression(std::string op, int id, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, id, childs) {}

        std::string interpret() override {
            return children->at(0)->interpret();
        }

        static const std::unordered_map<std::string, ExpressionPath> hash;
    };

    class SizeOptionExpression : public NonterminalExpression {
    public:
        SizeOptionExpression(std::string op, int d, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, d, childs) {}

        std::string interpret() override {
            return hash.at(op).condition;
        }

        static const std::unordered_map<std::string, ExpressionPath> hash;
    };

    class LengthExpression : public NonterminalExpression {
    public:
        LengthExpression(std::string op, int d, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, d, childs) {}

        std::string interpret() override {
            ComparatorExpression* comp = dynamic_cast<ComparatorExpression*>(children->at(0).get());
            comp->setOption(hash.at(op).condition);
            return children->at(0)->interpret();
        }

        //hash
        static const std::unordered_map<std::string, ExpressionPath> hash;
    };

    class ConvexEdgeOptionExpression : public NonterminalExpression {
    public:
        ConvexEdgeOptionExpression(std::string op, int d, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, d, childs) {}

        std::string interpret() override;

        static std::unordered_map<std::string, ExpressionPath> hash;
    };

    class ExtentOptionExpression : public NonterminalExpression {
    public:
        ExtentOptionExpression(std::string op, int d, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, d, childs) {}

        std::string interpret() override {
            return (id == 0) ? "0.1" : children->at(0)->interpret();
        }

        static const std::unordered_map<std::string, ExpressionPath> hash;
    };

    class RelationsOptionExpression : public NonterminalExpression {
    public:
        RelationsOptionExpression(std::string str, int id, std::vector<std::shared_ptr<Expression>> childs);

        std::string interpret() override;

        static const std::unordered_map<std::string, ExpressionPath> hash;
    };

    class LeftOptionExpression : public NonterminalExpression {
    public:
        LeftOptionExpression(std::string op, int d, std::vector<std::shared_ptr<Expression>> childs) : NonterminalExpression(op, d, childs) {}

        std::string interpret() override {
            return op;
        }

        static bool parser(const std::vector<std::string>& tokens, std::stack<std::shared_ptr<Expression>>& stack, int& i, const ExpressionPath& path);

        static const std::unordered_map<std::string, ExpressionPath> hash;
    };

    class LogicalExpression : public NonterminalExpression {
    public:
        LogicalExpression(std::string op, int d, std::vector<std::shared_ptr<Expression>> childs);

        std::string interpret() override;

        void RelationsInterpreter(std::ostringstream& result, int mode);

        void ConvexDetailInterpreter(std::ostringstream& result);

        void ConvexInterpreter(std::ostringstream& result);

        void GrowInterpreter(std::ostringstream& result);

        void setOption(int c, bool r = false, bool d = false);

        const std::string& GetCondition() const;

        void ANDSelfInterpreter(std::ostringstream& result);

        static const std::unordered_multimap<std::string, ExpressionPath> hash;
        static const std::vector<std::string> RelationOptionsList0;
        static const std::vector<std::string> RelationOptionsList1;
    };

    using ParserFunc = std::function<bool(const std::vector<std::string>&, std::stack<std::shared_ptr<Expression>>&, int&, const NonterminalExpression::ExpressionPath&)>;

    class Parser {
    private:
        static std::unordered_multimap<std::string, std::pair<ParserFunc, NonterminalExpression::ExpressionPath>> parserFuncMap;
        static std::unordered_map<std::string, std::function<bool(Expression*)>> isCorrectExpressionMap;
    public:
        static auto getIter(std::string str);

        template <typename T>
        static bool parser(const std::vector<std::string>& tokens, std::stack<std::shared_ptr<Expression>>& stack, int& i, const NonterminalExpression::ExpressionPath& path);

        static std::vector<std::string> tokenize(const std::string& str);

        static std::shared_ptr<Expression> parse(std::string& str);

        static std::shared_ptr<Expression> parse(const std::vector<std::string>& tokens);

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

