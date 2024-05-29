#ifndef EXPRESSIONENUM_H
#define EXPRESSIONENUM_H

namespace interpreter {
    enum class ExpressionType {
        ORIGIONAL,
        ASSIGN,
        AND,
        OR,
        NOT,
        XOR,
        NUMBER,
        CONSTRAINT,
        FILE_NAME,
        DATA_FILE_NAME,
        REPORT_FILE_NAME,
        SOURCE_NAME,
        LABELS_NAME,
        TARGET_NAME,
        REPORT_NAME,
        LAYER_NAME,
        REGION_LAYER_NAME,
        EDGE_LAYER_NAME,
        CELL_NAME,
        LEFT_OPTION,
        LEFT_ADD_OPTION,
        RELATIONS_OPTION,
        EXTENT,
        CONVEX_OPTIONS,
        LENGTH_OPTION,
        SIZE_OPTION,
        DIRECTION_OPTION,
        JOINED_OPTION,
        RESULT_TYPE_END
    };

    enum class NonTerminalExpressionType {
        ORIGIONAL,
        ASSIGN,
        //compare class
        EQUAL,
        NOT_EQUAL,
        GREATER,
        GREATER_EQUAL,
        LESS,
        LESS_EQUAL,
        RANGE,
        //logical class
        AND_SELF,
        AND,
        OR,
        NOT,
        XOR,
        ENCLOSE,
        INTERACT,
        LENGTH,
        GROW,
        SIZE,
        ANGLE,
        CONVEX,
        CONVEX_DETAIL,
        ENCLOSURE,
        INTERNAL,
        EXTERNAL,
        INTERNAL_SELF,
        EXTERNAL_SELF,
        AREA,
        INSIDE,
        OUTSIDE,
        INSIDE_EDGE,
        OUTSIDE_EDGE,
        COPY,
        DENSITY,
        COINCIDENT_EDGE,
        COINCIDENT_INSIDE_OR_OUTSIDE_EDGE,
        EXPAND_EDGE,
        OR_EDGE,
        POLYGON,
        RECTANGLE,
        ROTATE,
        SHIFT,
        SHRINK,
        TOUCH,
        TOUCH_EDGE,
        WITH_EDGE,
        EXTENT,
        //left class
        NOT_LEFT,
        EXCLUDE,
        //relationsoptions class
        OPPOSITE,
        SQUARE,
        SHILDED,
        PROJecting,
        REGION,
        //extent class
        EXTENTS,
        EXTENDED,
        //convex class
        ANGLE1,
        ANGLE2,
        WITH,
        EDGE,
        //length class
        LENGTH1,
        LENGTH2,
        LENGTH_OPTION,
        //size class
        OVERUNDER,
        UNDEROVER,
        //grow class
        LEFT,
        RIGHT,
        TOP,
        BOTTOM,
        //by class
        BY,
        //input class
        SOURCE,
        INPUT,
        LABELS,
        CELL,
        WINDOW,
        //output class
        OUTPUT,
        TARGET,
        NEW_TARGET,
        REPORT,
        NEW_REPORT,
        //
        CENTERS,
        INSIDE_OPTION,
        OF,
        LAYER_OPTION,
        FACTOR,
        EXTEND,
        CORNER,
        FILL,
        INSIDE_BY,
        OUTSIDE_BY,
        ALSO,
        ONLY,
        ENDPOINT,
        NTYPE_END
    };

    enum class ExpressionLimitType {
        EXPRESSION_TYPE_LIMIT,
        NEXPRESSION_TYPE_LIMIT,
        EXPRESSION_COUNT_LIMIT,
        EXPRESSION_NUMBER_LIMIT,
        NEXPRESSION_ONE_IN_N_LIMIT,
        LIMIT_TYPE_END
    };
}
#define INTERPRETERINITSTATE 0
#define INTERPRETERRUNNINGSTATE 1
#define INTERPRETERPAUSINGSTATE 2
#define INTERPRETERSTOPSTATE 3

#define LIMIT(b,  ...) std::make_shared<ExpressionOptionLimit>(b, std::unordered_map<ExpressionLimitType, std::any>{__VA_ARGS__})
#define EXPRESSIONTYPELIMIT(...) {ExpressionLimitType::EXPRESSION_TYPE_LIMIT, std::vector<ExpressionType>{__VA_ARGS__}}
#define NEXPRESSIONTYPELIMIT(...) {ExpressionLimitType::NEXPRESSION_TYPE_LIMIT, std::vector<NonTerminalExpressionType>{__VA_ARGS__}}
#define EXPRESSIONCOUNTLIMIT(n) {ExpressionLimitType::EXPRESSION_COUNT_LIMIT, n}
#define EXPRESSIONNUMBERLIMIT {ExpressionLimitType::EXPRESSION_NUMBER_LIMIT, 0}
#define NEXPRESSIONONEINNLIMIT(...) {ExpressionLimitType::NEXPRESSION_ONE_IN_N_LIMIT, std::vector<NonTerminalExpressionType>{__VA_ARGS__}}
#define LIMITLIST(...) std::make_shared<std::vector<std::shared_ptr<ExpressionOptionLimit>>>(std::vector<std::shared_ptr<ExpressionOptionLimit>>{__VA_ARGS__})
#define PREPTR std::make_shared<PreExpressionInfo>

struct  StructPoint{
    double x;
    double y;
    StructPoint(double x, double y) : x(x), y(y) {}
    StructPoint() : x(0), y(0) {}
};

using Point = StructPoint;



#endif