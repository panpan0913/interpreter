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
        CELL_NAME,
        LEFT_OPTION,
        RELATIONS_OPTION,
        EXTENT,
        CONVEX_OPTIONS,
        LENGTH_OPTION,
        SIZE_OPTION,
        GROW_OPTION,
        BY_OPTION,
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


#define LIMIT(b,  ...) std::make_shared<ExpressionOptionLimit>(b, std::unordered_map<ExpressionLimitType, std::any>{__VA_ARGS__})
#define LIMITLIST(...) std::make_shared<std::vector<std::shared_ptr<ExpressionOptionLimit>>>(std::vector<std::shared_ptr<ExpressionOptionLimit>>{__VA_ARGS__})
#define PREPTR std::make_shared<PreExpressionInfo>

#endif