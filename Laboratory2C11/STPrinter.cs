using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Antlr4.Runtime.Tree;
using System.Diagnostics;

namespace Laboratory2C11 {
    internal class STPrinter : C11ParserGrammarBaseVisitor<int> {
        private StreamWriter m_dotfile = new StreamWriter("ST.dot");
        private Stack<string> m_labelsStack = new Stack<string>();
        private static int ms_serialCounter = 0;

        public STPrinter() {

        }

        /*  (?!^)[A-Z][a-z]+(?![^A-Z_])  */

        public override int VisitChildren(IRuleNode node) {
            // Create label

            Type nodeType = node.GetType();
            string nodeTypeName = nodeType.Name;
            Match m = Regex.Match(nodeTypeName, @"\w+(?=Context)");

            string label = m.Value + ms_serialCounter++ ;

            if (m_labelsStack.Count > 0) {
                string parentLabel = m_labelsStack.Peek();

                // Print edge
                m_dotfile.WriteLine($"\"{parentLabel}\"->\"{label}\";");
            }
            else {
                m_dotfile.Write( "digraph G{" );
            }
            
            // Push label
            m_labelsStack.Push(label);

            // Visit children
            base.VisitChildren(node);

            // Pop label
            m_labelsStack.Pop();

            if (m_labelsStack.Count == 0) {
                m_dotfile.Write("}");
                m_dotfile.Close();

                ProcessStartInfo info = new ProcessStartInfo();
                info.FileName = "dot.exe";
                info.Arguments = "ST.dot -Tgif -o ST.gif";

                Process.Start(info);

                

            }
            

            return 0;
        }
    }
}
