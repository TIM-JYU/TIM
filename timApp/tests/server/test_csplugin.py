from timApp.answer.answer import Answer
from timApp.tests.server.timroutetest import TimRouteTest


class CsPluginTest(TimRouteTest):
    def test_csplugin_pointsrule(self):
        # Ensure csplugin is fully available since it's used in the plugin
        self.wait_for_url("http://csplugin:5000/cs/reqs")
        self.login_test1()
        d = self.create_doc(
            initial_par="""
``` {#t plugin="csPlugin"}
pointsRule:
    cumulative: true
    run: 0.1
    doc: 0.1
    doc_limit: 0.5
    allowUserMin: 0
    allowUserMax: 2.0
    output: 1.8
    expectOutputPlain: |!!
Hello
world
!!    
type: java/doc/comtest
byCode: |!!
!!
```"""
        )

        def pa(s, runtype="java", **kwargs):
            return self.post_answer(
                "csPlugin",
                f"{d.id}.t",
                user_input={
                    "type": runtype,
                    **kwargs,
                    "usercode": f"""
public class Main {{
    public static void main(String[] args) {{
        {s}
    }}
}}""",
                },
            )

        def first_answer():
            return self.test_user_1.answers.order_by(Answer.id.desc()).first()

        r = pa('System.out.println("hi");')
        self.assertEqual("hi\n", r["web"]["console"])
        self.assertEqual(0.1, first_answer().points)

        r = pa('System.out.println("hi");', document=True)
        self.assertEqual("", r["web"]["console"])
        self.assertEqual(0.1, first_answer().points)

        r = pa(r'System.out.println("Hello\nworld");')
        self.assertEqual("Hello\nworld\n", r["web"]["console"])
        self.assertEqual(1.9, first_answer().points)

        r = pa(r'System.out.println("Hello\nworld");', document=True)
        self.assertEqual("", r["web"]["console"])
        self.assertEqual(2, first_answer().points)

    def test_csplugin_csharp(self):
        # Ensure csplugin is fully available since it's used in the plugin
        self.wait_for_url("http://csplugin:5000/cs/reqs")
        self.login_test1()
        d = self.create_doc(
            initial_par="""
``` {#t plugin="csPlugin"}
type: cs
timeout: 20
```
        """
        )
        r = self.post_answer(
            "csPlugin",
            f"{d.id}.t",
            user_input={
                "type": "cs",
                "usercode": """
public class Run {
    public static void Main(string[] args) {
        System.Console.WriteLine("Hello world");
    }
}""",
            },
        )
        self.assertEqual("Hello world\n", r["web"]["console"])
