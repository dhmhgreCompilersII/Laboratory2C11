using System.Collections.Generic;
using System.ComponentModel;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using Laboratory2C11;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Hosting;

public sealed class StartupSettings {
    public List<string> Settings { get; set; }
}

public class CompilerFacade {
    private List<string> m_inputFiles;
    private string m_configurationFileDirectory;

    public CompilerFacade(string configurationFileDirectory) {
        m_configurationFileDirectory = configurationFileDirectory;
        
        // Build a config object, using env vars and JSON providers.
        IConfiguration config = new ConfigurationBuilder()
            .AddJsonFile($@"{configurationFileDirectory}\\appsettings.json")
            .AddEnvironmentVariables()
            .Build();

        // Get values from the config given their key and their target type.
        m_inputFiles = config
            .GetRequiredSection("StartupSettings")
            .GetSection("InputFiles")
            .Get<List<string>>();

        foreach (string filename in m_inputFiles) {
            Console.WriteLine($"File = {filename}");
        }
    }

    public void Parse() {
        foreach (string inputFile in m_inputFiles) {
            StreamReader astream = new StreamReader(inputFile);
            AntlrInputStream antlrInputStream = new AntlrInputStream(astream);
            C11LexerGrammar lexer=  new C11LexerGrammar( antlrInputStream );
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            C11ParserGrammar parser = new C11ParserGrammar(tokens);
            IParseTree tree=parser.translationUnit();
            STPrinter stPrinter = new STPrinter();
            stPrinter.Visit(tree);
        }
    }
}