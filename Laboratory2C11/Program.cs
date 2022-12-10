using Microsoft.Extensions.Hosting;

using IHost host = Host.CreateDefaultBuilder(args).Build();

// This will get the current WORKING directory (i.e. \bin\Debug)
string workingDirectory = Environment.CurrentDirectory;

// This will get the current PROJECT bin directory (ie ../bin/)
string projectDirectory = Directory.GetParent(workingDirectory).Parent.Parent.FullName;

// Application code should start here.
CompilerFacade facade = new CompilerFacade(projectDirectory);

facade.Parse();
await host.RunAsync(); 