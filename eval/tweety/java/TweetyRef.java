// Reference answers from TweetyProject 1.31 for the example theories.
// java TweetyRef aspic|delp|aba|apx FILE [QUERY ...]
import java.util.*;
import org.tweetyproject.arg.aspic.parser.AspicParser;
import org.tweetyproject.arg.aspic.reasoner.SimpleAspicReasoner;
import org.tweetyproject.arg.aspic.ruleformulagenerator.PlFormulaGenerator;
import org.tweetyproject.arg.aspic.syntax.AspicArgumentationTheory;
import org.tweetyproject.arg.aba.parser.AbaParser;
import org.tweetyproject.arg.aba.reasoner.FlatAbaReasoner;
import org.tweetyproject.arg.aba.syntax.AbaTheory;
import org.tweetyproject.arg.delp.parser.DelpParser;
import org.tweetyproject.arg.delp.reasoner.DelpReasoner;
import org.tweetyproject.arg.delp.semantics.GeneralizedSpecificity;
import org.tweetyproject.arg.delp.syntax.DefeasibleLogicProgram;
import org.tweetyproject.arg.dung.parser.ApxParser;
import org.tweetyproject.arg.dung.reasoner.AbstractExtensionReasoner;
import org.tweetyproject.arg.dung.semantics.Semantics;
import org.tweetyproject.arg.dung.syntax.DungTheory;
import org.tweetyproject.commons.InferenceMode;
import org.tweetyproject.logics.fol.syntax.FolFormula;
import org.tweetyproject.logics.pl.parser.PlParser;
import org.tweetyproject.logics.pl.sat.Sat4jSolver;
import org.tweetyproject.logics.pl.sat.SatSolver;
import org.tweetyproject.logics.pl.syntax.PlFormula;

public class TweetyRef {
  static final Semantics[] SEMS = {Semantics.GROUNDED_SEMANTICS, Semantics.COMPLETE_SEMANTICS, Semantics.PREFERRED_SEMANTICS, Semantics.STABLE_SEMANTICS};
  public static void main(String[] a) throws Exception {
    SatSolver.setDefaultSolver(new Sat4jSolver());
    String mode = a[0], file = a[1];
    Semantics[] sems = System.getenv("TWEETY_SEMS") == null ? SEMS : java.util.Arrays.stream(System.getenv("TWEETY_SEMS").split(",")).map(Semantics::valueOf).toArray(Semantics[]::new);
    if (mode.equals("aspic")) {
      PlParser pl = new PlParser();
      AspicParser<PlFormula> p = new AspicParser<>(pl, new PlFormulaGenerator());
      AspicArgumentationTheory<PlFormula> at = p.parseBeliefBaseFromFile(file);
      System.out.println("arguments: " + at.getArguments().size());
      for (int i = 2; i < a.length; i++) {
        PlFormula q = (PlFormula) pl.parseFormula(a[i]);
        StringBuilder sb = new StringBuilder(a[i] + ":");
        for (Semantics s : sems) {
          SimpleAspicReasoner<PlFormula> r = new SimpleAspicReasoner<>(AbstractExtensionReasoner.getSimpleReasonerForSemantics(s));
          sb.append(" ").append(s.abbreviation()).append("=").append(r.query(at, q, InferenceMode.CREDULOUS) ? "c" : "-").append(r.query(at, q, InferenceMode.SKEPTICAL) ? "s" : "-");
        }
        System.out.println(sb);
      }
    } else if (mode.equals("aspicfol")) {
      // a[2]: the signature file; the queries follow
      org.tweetyproject.logics.fol.parser.FolParser fp = new org.tweetyproject.logics.fol.parser.FolParser();
      fp.setSignature(fp.parseSignature(new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(a[2])))));
      AspicParser<FolFormula> p = new AspicParser<>(fp, new org.tweetyproject.arg.aspic.ruleformulagenerator.FolFormulaGenerator());
      p.setSymbolComma(";");
      AspicArgumentationTheory<FolFormula> at = p.parseBeliefBaseFromFile(file);
      System.out.println("arguments: " + at.getArguments().size() + " attacks: " + at.asDungTheory().getAttacks().size());
      for (int i = 3; i < a.length; i++) {
        FolFormula q = (FolFormula) fp.parseFormula(a[i]);
        StringBuilder sb = new StringBuilder(a[i] + ":");
        for (Semantics s : SEMS) {
          SimpleAspicReasoner<FolFormula> r = new SimpleAspicReasoner<>(AbstractExtensionReasoner.getSimpleReasonerForSemantics(s));
          sb.append(" ").append(s.abbreviation()).append("=").append(r.query(at, q, InferenceMode.CREDULOUS) ? "c" : "-").append(r.query(at, q, InferenceMode.SKEPTICAL) ? "s" : "-");
        }
        System.out.println(sb);
      }
    } else if (mode.equals("delp")) {
      DelpParser p = new DelpParser();
      DefeasibleLogicProgram d = p.parseBeliefBaseFromFile(file);
      DelpReasoner r = new DelpReasoner(new GeneralizedSpecificity());
      for (int i = 2; i < a.length; i++)
        System.out.println(a[i] + ": " + r.query(d, (FolFormula) p.parseFormula(a[i])));
    } else if (mode.equals("aba")) {
      AbaParser<PlFormula> p = new AbaParser<>(new PlParser());
      AbaTheory<PlFormula> t = p.parseBeliefBaseFromFile(file);
      System.out.println("parsed: " + t);
      for (Semantics s : SEMS)
        System.out.println(s.abbreviation() + ": " + new FlatAbaReasoner<PlFormula>(s).getModels(t));
    } else if (mode.equals("af")) {
      // a[1]: arguments "a,b,c"; a[2]: attacks "a>b,b>c"
      DungTheory t = new DungTheory();
      java.util.Map<String, org.tweetyproject.arg.dung.syntax.Argument> m = new java.util.HashMap<>();
      for (String n : a[1].split(",")) if (!n.isEmpty()) { m.put(n, new org.tweetyproject.arg.dung.syntax.Argument(n)); t.add(m.get(n)); }
      if (a.length > 2) for (String e : a[2].split(",")) if (!e.isEmpty()) { String[] p = e.split(">"); t.add(new org.tweetyproject.arg.dung.syntax.Attack(m.get(p[0]), m.get(p[1]))); }
      for (Semantics s : SEMS)
        System.out.println(s.abbreviation() + ": " + AbstractExtensionReasoner.getSimpleReasonerForSemantics(s).getModels(t));
    } else if (mode.equals("rdl")) {
      org.tweetyproject.logics.rdl.parser.RdlParser p = new org.tweetyproject.logics.rdl.parser.RdlParser();
      org.tweetyproject.logics.rdl.syntax.DefaultTheory t = p.parseBeliefBaseFromFile(file);
      org.tweetyproject.logics.rdl.reasoner.SimpleDefaultReasoner r = new org.tweetyproject.logics.rdl.reasoner.SimpleDefaultReasoner();
      for (org.tweetyproject.logics.rdl.semantics.Extension e : r.getModels(t))
        System.out.println("extension: " + new java.util.ArrayList<>(e));
      for (int i = 2; i < a.length; i++)
        System.out.println(a[i] + ": credulous " + r.query(t, (FolFormula) p.parseFormula(a[i]), InferenceMode.CREDULOUS) + ", skeptical " + r.query(t, (FolFormula) p.parseFormula(a[i]), InferenceMode.SKEPTICAL));
    } else if (mode.equals("apx")) {
      DungTheory t = new ApxParser().parseBeliefBaseFromFile(file);
      for (Semantics s : SEMS)
        System.out.println(s.abbreviation() + ": " + AbstractExtensionReasoner.getSimpleReasonerForSemantics(s).getModels(t));
    }
  }
}
