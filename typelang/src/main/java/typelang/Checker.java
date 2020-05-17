package typelang;

import java.util.ArrayList;
import java.util.List;

import com.sun.org.apache.xpath.internal.operations.Bool;
import typelang.AST.*;
import typelang.Env.ExtendEnv;
import typelang.Env.GlobalEnv;
import typelang.Type.*;

public class Checker implements Visitor<Type,Env<Type>> {
	Printer.Formatter ts = new Printer.Formatter();
	Env<Type> initEnv = initialEnv(); //New for definelang
	
	private Env<Type> initialEnv() {
		GlobalEnv<Type> initEnv = new GlobalEnv<Type>();
		
		/* Type for procedure: (read <filename>). Following is same as (define read (lambda (file) (read file))) */
		List<Type> formalTypes = new ArrayList<Type>();
		formalTypes.add(new Type.StringT());
		initEnv.extend("read", new Type.FuncT(formalTypes, new Type.StringT()));

		/* Type for procedure: (require <filename>). Following is same as (define require (lambda (file) (eval (read file)))) */
		formalTypes = new ArrayList<Type>();
		formalTypes.add(new Type.StringT());
		initEnv.extend("eval", new Type.FuncT(formalTypes, new Type.UnitT()));
		
		/* Add type for new built-in procedures here */ 
		
		return initEnv;
	}
	
    Type check(Program p) {
		return (Type) p.accept(this, null);
	}

	public Type visit(Program p, Env<Type> env) {
		Env<Type> new_env = initEnv;

		for (DefineDecl d: p.decls()) {
			Type type = (Type)d.accept(this, new_env);

			if (type instanceof ErrorT) { return type; }

			Type dType = d.type();

			if (!type.typeEqual(dType)) {
				return new ErrorT("Expected " + dType + " found " + type + " in " + ts.visit(d, null));
			}

			new_env = new ExtendEnv<Type>(new_env, d.name(), dType);
		}
		return (Type) p.e().accept(this, new_env);
	}

	public Type visit(VarExp e, Env<Type> env) {
		try {
			return env.get(e.name());
		} catch(Exception ex) {
			return new ErrorT("Variable " + e.name() +
					" has not been declared in " + ts.visit(e, null));
		}
	}

	public Type visit(LetExp e, Env<Type> env) {
		// answer question 6
		for (int i = 0; i < e.value_exps().size(); i++) {
			Exp ei = e.value_exps().get(i);
			Type ti = e.varTypes().get(i);
			Type vT = (Type)ei.accept(this, env);
			String vari = e.names().get(i);
			if(vT instanceof ErrorT) return vT;
			if(!ti.typeEqual(vT)) return new ErrorT("The declared type of the " + i + " let variable and the actual type mismatch, expect " + ti.tostring() + ", found " + vT.tostring() + " in " + ts.visit(e, null));
			ExtendEnv ext = new ExtendEnv(env, vari, ti);
			env = ext;

		}
		Exp body = e.body();
		Type T = (Type)body.accept(this, env);
		return T;
	}

	public Type visit(DefineDecl d, Env<Type> env) {
		String name = d.name();
		Type t =(Type) d._value_exp.accept(this, env);
		((GlobalEnv<Type>) initEnv).extend(name, t);
		return t;
	}

	public Type visit(LambdaExp e, Env<Type> env) {
		List<String> names = e.formals();
		List<Type> types = e.types();
		String message = "The number of formal parameters and the number of "
				+ "arguments in the function type do not match in ";
		if (types.size() == names.size()) {
			Env<Type> new_env = env;
			int index = 0;
			for (Type argType : types) {
				new_env = new ExtendEnv<Type>(new_env, names.get(index),
						argType);
				index++;
			}

			Type bodyType = (Type) e.body().accept(this, new_env);
			return new FuncT(types,bodyType);
		}
		return new ErrorT(message + ts.visit(e, null));
	}

	public Type visit(CallExp e, Env<Type> env) {
		// answer question 7
		Exp ef = e.operator();
		Type tf = (Type)ef.accept(this, env);
		if(tf instanceof ErrorT) return tf;
		if(!(tf instanceof FuncT)) return new ErrorT("Expected a function type, found " + tf.tostring() + " in " + ts.visit(e, null));
		List<Type> argTypes = ((FuncT) tf).argTypes();
		Type tb = ((FuncT) tf).returnType();
		for (int i = 0; i <e.operands().size() ; i++) {
			Exp ei = e.operands().get(i);
			Type et = (Type)ei.accept(this, env);
			Type ti = argTypes.get(i);
			if(!et.typeEqual(ti)) return new ErrorT("The expected type of parameter " + i + " is " + ti.tostring() + ", found " + et.tostring() + " in " + ts.visit(e, null));
		}
		return tb;
	}

	public Type visit(IfExp e, Env<Type> env) {
		// answer question 5
        Exp condE  = e.conditional();
        Exp thenE = e.then_exp();
        Exp elseE = e.else_exp();
        Type condT = (Type)condE.accept(this, env);
        Type thenT = (Type)thenE.accept(this, env);
        Type elseT = (Type)elseE.accept(this, env);
        if(condT instanceof ErrorT) return condT;
        if(!(condT instanceof BoolT)) return new ErrorT("The condition should have boolean type, found" + condT.tostring() + " in " + ts.visit(e, null));
        if(thenT instanceof ErrorT) return thenT;
        if(elseT instanceof ErrorT) return elseT;
        if(thenT.typeEqual(elseT)) return thenT;
		return new ErrorT("The then and else expressions should have the same type, then has type " + thenT.tostring() + " else has type " + elseT.tostring() + " in " + ts.visit(e, null));
	}

	public Type visit(CarExp e, Env<Type> env) {
		Exp exp = e.arg();
		Type type = (Type)exp.accept(this, env);
		if (type instanceof ErrorT) { return type; }

		if (type instanceof PairT) {
			PairT pt = (PairT)type;
			return pt.fst();
		}
		return new ErrorT("The car expect an expression of type Pair, found "
				+ type.tostring() + " in " + ts.visit(e, null));
	}

	public Type visit(CdrExp e, Env<Type> env) {
		// answer question 2(a)
		Exp e1 = e.arg();
		Type t1 = (Type)e1.accept(this, env);
		if(t1 instanceof ErrorT) return t1;
		if(t1 instanceof PairT){
			return (Type)((ListExp) e1).elems().get(1).accept(this, env);
		}
		return new ErrorT("The cdr expects an expression of type Pair, found " + t1.tostring() + " in " + ts.visit(e, null));
	}

	public Type visit(ConsExp e, Env<Type> env) {
		Exp fst = e.fst(); 
		Exp snd = e.snd();

		Type t1 = (Type)fst.accept(this, env);
		if (t1 instanceof ErrorT) { return t1; }

		Type t2 = (Type)snd.accept(this, env);
		if (t2 instanceof ErrorT) { return t2; }

		return new PairT(t1, t2);
	}

	public Type visit(ListExp e, Env<Type> env) {
		// answer question 2(b)
		Type T = e.type();
		for (int i = 0; i < e.elems().size(); i++) {
			Exp ei = e.elems().get(i);
			Type ti = (Type)ei.accept(this, env);
			if(ti instanceof ErrorT) return ti;
			if(!ti.typeEqual(T)) return new ErrorT("The " + i +
					" expression should have type " + T.tostring() + " found " + ti.tostring() + " in " + ts.visit(e, null));
		}
		return new ListT(T);
	}

	public Type visit(NullExp e, Env<Type> env) {
		Exp arg = e.arg();
		Type type = (Type)arg.accept(this, env);
		if (type instanceof ErrorT) { return type; }

		if (type instanceof ListT) { return BoolT.getInstance(); }

		return new ErrorT("The null? expect an expression of type List, found "
				+ type.tostring() + " in " + ts.visit(e, null));
	}

	public Type visit(RefExp e, Env<Type> env) {
		// answer question 1(a)
		Exp e1 = e.value_exp();
		Type expected = e.type();
		Type type = (Type)e1.accept(this, env);
		if (type instanceof ErrorT) { return type; }
		else if (type.typeEqual( expected)) { return new RefT(type); }
		return new ErrorT("The Ref expression expect type " + expected.tostring() + " found " + type.tostring() + " in  " + ts.visit(e, null));
	}

	public Type visit(DerefExp e, Env<Type> env) {
		Exp exp = e.loc_exp();
		Type type = (Type)exp.accept(this, env);
		if (type instanceof ErrorT) { return type; }

		if (type instanceof RefT) {
			RefT rt = (RefT)type;
			return rt.nestType();
		}

		return new ErrorT("The dereference expression expect a reference type " +
				"found " + type.tostring() + " in " + ts.visit(e, null));
	}

	public Type visit(AssignExp e, Env<Type> env) {
		// answer question 1(b)
		Exp e1 = e.lhs_exp();
		Exp e2 = e.rhs_exp();
		Type t1 = (Type)e1.accept(this, env);
		Type t2 = (Type)e2.accept(this, env);
		if(t1 instanceof ErrorT) return t1;
		if(t1 instanceof RefT && ((RefT) t1).nestType() != null){
			Type expected = ((RefT) t1).nestType();
			if(t2 instanceof ErrorT) return t2;
			if(t2.typeEqual(expected)){}
			return new ErrorT("The inner type of the reference type is " + expected.tostring()+
					" the rhs type is " + t2.tostring() + " in " + ts.visit(e, null));
		}
		return new ErrorT("The lhs of the assignment expression expected a reference type found " + t1.tostring() + " in " + ts.visit(e, null));
	}

	public Type visit(FreeExp e, Env<Type> env) {
		Exp exp = e.value_exp();
		Type type = (Type)exp.accept(this, env);

		if (type instanceof ErrorT) { return type; }

		if (type instanceof RefT) { return UnitT.getInstance(); }

		return new ErrorT("The free expression expect a reference type " +
				"found " + type.tostring() + " in " + ts.visit(e, null));
	}

	public Type visit(UnitExp e, Env<Type> env) {
		return Type.UnitT.getInstance();
	}

	public Type visit(NumExp e, Env<Type> env) {
		return NumT.getInstance();
	}

	public Type visit(StrExp e, Env<Type> env) {
		return Type.StringT.getInstance();
	}

	public Type visit(BoolExp e, Env<Type> env) {
		return Type.BoolT.getInstance();
	}

	public Type visit(LessExp e, Env<Type> env) {
		return visitBinaryComparator(e, env, ts.visit(e, null));
	}

	public Type visit(EqualExp e, Env<Type> env) {
		return visitBinaryComparator(e, env, ts.visit(e, null));
	}

	public Type visit(GreaterExp e, Env<Type> env) {
		return visitBinaryComparator(e, env, ts.visit(e, null));
	}

	private Type visitBinaryComparator(BinaryComparator e, Env<Type> env,
			String printNode) {
		// answer question 4
		Exp e1 = e.first_exp();
		Exp e2 = e.second_exp();
		Type t1 = (Type)e1.accept(this, env);
		Type t2 = (Type)e2.accept(this, env);
		if(!(t1 instanceof NumT)) return new ErrorT("The first argument of a binary expression should be num Type, found " + t1.tostring() + " in " + printNode);
		if(!(t2 instanceof NumT)) return new ErrorT("The second argument of a binary expression should be num Type, found " + t2.tostring() + " in " + printNode);

		return BoolT.getInstance();
	}


	public Type visit(AddExp e, Env<Type> env) {
		return visitCompoundArithExp(e, env, ts.visit(e, null));
	}

	public Type visit(DivExp e, Env<Type> env) {
		return visitCompoundArithExp(e, env, ts.visit(e, null));
	}

	public Type visit(MultExp e, Env<Type> env) {
		return visitCompoundArithExp(e, env, ts.visit(e, null));
	}

	public Type visit(SubExp e, Env<Type> env) {
		return visitCompoundArithExp(e, env, ts.visit(e, null));
	}

	private Type visitCompoundArithExp(CompoundArithExp e, Env<Type> env, String printNode) {
		// answer question 3

		List<Exp> Es = e.all();
		for (int i = 0; i <Es.size() ; i++) {
			Exp ei = Es.get(i);
			Type ti = (Type)ei.accept(this, env);
			if(ti instanceof ErrorT) return ti;
			if(!(ti instanceof NumT)) return new ErrorT("Expected num, found " + ti.tostring() + " in " + printNode);
		}
		return NumT.getInstance();
	}

	private static boolean assignable(Type t1, Type t2) {
		if (t2 instanceof UnitT) { return true; }

		return t1.typeEqual(t2);
	}
	
	public Type visit(ReadExp e, Env<Type> env) {
		return UnitT.getInstance();
	}

	public Type visit(EvalExp e, Env<Type> env) {
		return UnitT.getInstance();
	}
}
