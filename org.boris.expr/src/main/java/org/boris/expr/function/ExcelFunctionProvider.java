/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.expr.function;

import java.util.HashMap;
import java.util.Map;

import org.boris.expr.Expr;
import org.boris.expr.ExprException;
import org.boris.expr.ExprFunction;
import org.boris.expr.IExprFunction;
import org.boris.expr.function.excel.*;

public class ExcelFunctionProvider implements IFunctionProvider
{
    private static Map<String, IExprFunction> functions = new HashMap();

    static {
        functions.put("ABS", new ABS());
        functions.put("ACOS", new ACOS());
        functions.put("ACOSH", new ACOSH());
        functions.put("ADDRESS", new ADDRESS());
        functions.put("AND", new AND());
        functions.put("AREAS", new AREAS());
        functions.put("ASIN", new ASIN());
        functions.put("ASINH", new ASINH());
        functions.put("ATAN", new ATAN());
        functions.put("ATAN2", new ATAN2());
        functions.put("ATANH", new ATANH());
        functions.put("AVEDEV", new AVEDEV());
        functions.put("AVERAGE", new AVERAGE());
        functions.put("AVERAGEA", new AVERAGEA());
        functions.put("BAHTTEXT", new BAHTTEXT());
        functions.put("BETADIST", new BETADIST());
        functions.put("BETAINV", new BETAINV());
        functions.put("BINOMDIST", new BINOMDIST());
        functions.put("CEILING", new CEILING());
        functions.put("CELL", new CELL());
        functions.put("CHAR", new CHAR());
        functions.put("CHIDIST", new CHIDIST());
        functions.put("CHIINV", new CHIINV());
        functions.put("CHITEST", new CHITEST());
        functions.put("CHOOSE", new CHOOSE());
        functions.put("CLEAN", new CLEAN());
        functions.put("CODE", new CODE());
        functions.put("COLUMN", new COLUMN());
        functions.put("COLUMNS", new COLUMNS());
        functions.put("COMBIN", new COMBIN());
        functions.put("CONCATENATE", new CONCATENATE());
        functions.put("CONFIDENCE", new CONFIDENCE());
        functions.put("CORREL", new CORREL());
        functions.put("COS", new COS());
        functions.put("COSH", new COSH());
        functions.put("COUNT", new COUNT());
        functions.put("COUNTA", new COUNTA());
        functions.put("COUNTBLANK", new COUNTBLANK());
        functions.put("COUNTIF", new COUNTIF());
        functions.put("COVAR", new COVAR());
        functions.put("CRITBINOM", new CRITBINOM());
        functions.put("DATE", new DATE());
        functions.put("DATEVALUE", new DATEVALUE());
        functions.put("DAVERAGE", new DAVERAGE());
        functions.put("DAY", new DAY());
        functions.put("DAYS360", new DAYS360());
        functions.put("DB", new DB());
        functions.put("DCOUNT", new DCOUNT());
        functions.put("DCOUNTA", new DCOUNTA());
        functions.put("DDB", new DDB());
        functions.put("DEGREES", new DEGREES());
        functions.put("DEVSQ", new DEVSQ());
        functions.put("DGET", new DGET());
        functions.put("DMAX", new DMAX());
        functions.put("DMIN", new DMIN());
        functions.put("DOLLAR", new DOLLAR());
        functions.put("DPRODUCT", new DPRODUCT());
        functions.put("DSTDEV", new DSTDEV());
        functions.put("DSTDEVP", new DSTDEVP());
        functions.put("DSUM", new DSUM());
        functions.put("DVAR", new DVAR());
        functions.put("DVARP", new DVARP());
        functions.put("ERROR.TYPE", new ERRORTYPE());
        functions.put("EVEN", new EVEN());
        functions.put("EXACT", new EXACT());
        functions.put("EXP", new EXP());
        functions.put("EXPONDIST", new EXPONDIST());
        functions.put("FACT", new FACT());
        functions.put("FALSE", new FALSE());
        functions.put("FDIST", new FDIST());
        functions.put("FIND", new FIND());
        functions.put("FINV", new FINV());
        functions.put("FISHER", new FISHER());
        functions.put("FISHERNV", new FISHERNV());
        functions.put("FIXED", new FIXED());
        functions.put("FLOOR", new FLOOR());
        functions.put("FORECAST", new FORECAST());
        functions.put("FREQUENCY", new FREQUENCY());
        functions.put("FTEST", new FTEST());
        functions.put("FV", new FV());
        functions.put("GAMMADIST", new GAMMADIST());
        functions.put("GAMMAINV", new GAMMAINV());
        functions.put("GAMMALN", new GAMMALN());
        functions.put("GEOMEAN", new GEOMEAN());
        functions.put("GETPIVOTDATA", new GETPIVOTDATA());
        functions.put("GROWTH", new GROWTH());
        functions.put("HARMEAN", new HARMEAN());
        functions.put("HLOOKUP", new HLOOKUP());
        functions.put("HOUR", new HOUR());
        functions.put("HYPERLINK", new HYPERLINK());
        functions.put("HYPGEOMDIST", new HYPGEOMDIST());
        functions.put("IF", new IF());
        functions.put("INDEX", new INDEX());
        functions.put("INDIRECT", new INDIRECT());
        functions.put("INFO", new INFO());
        functions.put("INT", new INT());
        functions.put("INTERCEPT", new INTERCEPT());
        functions.put("IPMT", new IPMT());
        functions.put("IRR", new IRR());
        functions.put("ISBLANK", new ISBLANK());
        functions.put("ISERR", new ISERR());
        functions.put("ISLOGICAL", new ISLOGICAL());
        functions.put("ISNA", new ISNA());
        functions.put("ISNONTEXT", new ISNONTEXT());
        functions.put("ISNUMBER", new ISNUMBER());
        functions.put("ISPMT", new ISPMT());
        functions.put("ISREF", new ISREF());
        functions.put("ISTEXT", new ISTEXT());
        functions.put("KURT", new KURT());
        functions.put("LARGE", new LARGE());
        functions.put("LEFT", new LEFT());
        functions.put("LEN", new LEN());
        functions.put("LINEST", new LINEST());
        functions.put("LN", new LN());
        functions.put("LOG", new LOG());
        functions.put("LOG10", new LOG10());
        functions.put("LOGEST", new LOGEST());
        functions.put("LOGINV", new LOGINV());
        functions.put("LOGNORMDIST", new LOGNORMDIST());
        functions.put("LOOKUP", new LOOKUP());
        functions.put("LOWER", new LOWER());
        functions.put("MATCH", new MATCH());
        functions.put("MAX", new MAX());
        functions.put("MAXA", new MAXA());
        functions.put("MDETERM", new MDETERM());
        functions.put("MEDIAN", new MEDIAN());
        functions.put("MID", new MID());
        functions.put("MIN", new MIN());
        functions.put("MINA", new MINA());
        functions.put("MINUTE", new MINUTE());
        functions.put("MINVERSE", new MINVERSE());
        functions.put("MIRR", new MIRR());
        functions.put("MMULT", new MMULT());
        functions.put("MOD", new MOD());
        functions.put("MODE", new MODE());
        functions.put("MONTH", new MONTH());
        functions.put("N", new N());
        functions.put("NA", new NA());
        functions.put("NEGBINOMDIST", new NEGBINOMDIST());
        functions.put("NORMDIST", new NORMDIST());
        functions.put("NORMINV", new NORMINV());
        functions.put("NORMSDIST", new NORMSDIST());
        functions.put("NORMSINV", new NORMSINV());
        functions.put("NOT", new NOT());
        functions.put("NOW", new NOW());
        functions.put("NPER", new NPER());
        functions.put("NPV", new NPV());
        functions.put("ODD", new ODD());
        functions.put("OFFSET", new OFFSET());
        functions.put("OR", new OR());
        functions.put("PEARSON", new PEARSON());
        functions.put("PERCENTILE", new PERCENTILE());
        functions.put("PERCENTRANK", new PERCENTRANK());
        functions.put("PERMUT", new PERMUT());
        functions.put("PI", new PI());
        functions.put("PMT", new PMT());
        functions.put("POISSON", new POISSON());
        functions.put("POWER", new POWER());
        functions.put("PPMT", new PPMT());
        functions.put("PROB", new PROB());
        functions.put("PRODUCT", new PRODUCT());
        functions.put("PROPER", new PROPER());
        functions.put("PV", new PV());
        functions.put("QUARTILE", new QUARTILE());
        functions.put("RADIANS", new RADIANS());
        functions.put("RAND", new RAND());
        functions.put("RANK", new RANK());
        functions.put("RATE", new RATE());
        functions.put("REPLACE", new REPLACE());
        functions.put("REPT", new REPT());
        functions.put("RIGHT", new RIGHT());
        functions.put("ROMAN", new ROMAN());
        functions.put("ROUND", new ROUND());
        functions.put("ROUNDDOWN", new ROUNDDOWN());
        functions.put("ROUNDUP", new ROUNDUP());
        functions.put("ROW", new ROW());
        functions.put("ROWS", new ROWS());
        functions.put("RSQ", new RSQ());
        functions.put("RTD", new RTD());
        functions.put("SEARCH", new SEARCH());
        functions.put("SECOND", new SECOND());
        functions.put("SIGN", new SIGN());
        functions.put("SIN", new SIN());
        functions.put("SINH", new SINH());
        functions.put("SKEW", new SKEW());
        functions.put("SLN", new SLN());
        functions.put("SLOPE", new SLOPE());
        functions.put("SMALL", new SMALL());
        functions.put("SQRT", new SQRT());
        functions.put("STANDARDIZE", new STANDARDIZE());
        functions.put("STDEV", new STDEV());
        functions.put("STDEVA", new STDEVA());
        functions.put("STDEVP", new STDEVP());
        functions.put("STDEVPA", new STDEVPA());
        functions.put("STEYX", new STEYX());
        functions.put("SUBSTITUTE", new SUBSTITUTE());
        functions.put("SUBTOTAL", new SUBTOTAL());
        functions.put("SUM", new SUM());
        functions.put("SUMIF", new SUMIF());
        functions.put("SUMPRODUCT", new SUMPRODUCT());
        functions.put("SUMSQ", new SUMSQ());
        functions.put("SUMX2MY2", new SUMX2MY2());
        functions.put("SUMX2PY2", new SUMX2PY2());
        functions.put("SUMXMY2", new SUMXMY2());
        functions.put("SYD", new SYD());
        functions.put("T", new T());
        functions.put("TAN", new TAN());
        functions.put("TANH", new TANH());
        functions.put("TDIST", new TDIST());
        functions.put("TEXT", new TEXT());
        functions.put("TIME", new TIME());
        functions.put("TIMEVALUE", new TIMEVALUE());
        functions.put("TINV", new TINV());
        functions.put("TODAY", new TODAY());
        functions.put("TRANSPOSE", new TRANSPOSE());
        functions.put("TREND", new TREND());
        functions.put("TRIM", new TRIM());
        functions.put("TRIMMEAN", new TRIMMEAN());
        functions.put("TRUE", new TRUE());
        functions.put("TRUNC", new TRUNC());
        functions.put("TTEST", new TTEST());
        functions.put("TYPE", new TYPE());
        functions.put("UPPER", new UPPER());
        functions.put("VALUE", new VALUE());
        functions.put("VAR", new VAR());
        functions.put("VARA", new VARA());
        functions.put("VARP", new VARP());
        functions.put("VARPA", new VARPA());
        functions.put("VDB", new VDB());
        functions.put("VLOOKUP", new VLOOKUP());
        functions.put("WEEKDAY", new WEEKDAY());
        functions.put("WEIBULL", new WEIBULL());
        functions.put("YEAR", new YEAR());
        functions.put("ZTEST", new ZTEST());
    }

    public Expr evaluate(String name, Expr[] args) throws ExprException {
        IExprFunction ef = functions.get(name.toUpperCase());
        if (ef != null)
            return ef.evaluate(args);
        return null;
    }

    public boolean hasFunction(ExprFunction function) {
        return functions.containsKey(function.getName().toUpperCase());
    }

    public Expr evaluate(ExprFunction function) throws ExprException {
        IExprFunction f = functions.get(function.getName().toUpperCase());
        if (f != null)
            return f.evaluate(function.getArgs());
        return null;
    }
}
