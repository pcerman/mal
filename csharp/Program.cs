using System;
using System.Collections.Generic;

namespace Mal
{
    class Program
    {
        private static bool gArg_Help = false;
        private static string gArg_Step = "";

        internal static string gArg_File = null;
        internal static string[] gArgv = new string[0];

        private static void ShowUsage()
        {
            Console.Error.WriteLine("    use: mal {option}* <input_file>*\n" +
                                    "options: -help                show this help\n" +
                                    "         -step <step_id>      start step version of the mal");
        }

        private static bool ParseArguments(string[] args)
        {
            List<string> argv = new List<string>();

            string lastOption = null;
            bool getArgument = false;

            for (int i = 0; i < args.Length; i++)
            {
                string arg = args[i];
                if (string.IsNullOrEmpty(arg))
                    continue;

                if (arg[0] == '-')
                {
                    if (getArgument)
                    {
                        Console.Error.WriteLine("Error: argument for option '{0}' is required", lastOption);
                        return false;
                    }

                    lastOption = arg;

                    switch (arg)
                    {
                        case "-step":
                            getArgument = true;
                            break;

                        case "-h":
                        case "-help":
                            gArg_Help = true;
                            return false;

                        default:
                            Console.Error.WriteLine("Error: unknown option '{0}'", arg);
                            return false;
                    }
                }
                else if (getArgument)
                {
                    bool invalidArg = false;

                    switch (lastOption)
                    {
                        case "-step":
                            gArg_Step = arg;
                            break;

                        default:
                            invalidArg = true;
                            break;
                    }

                    if (invalidArg)
                    {
                        Console.Error.WriteLine("Error: invalid value for option '{0}'", lastOption);
                        return false;
                    }

                    lastOption = "";
                    getArgument = false;
                }
                else
                {
                    if (string.IsNullOrEmpty(gArg_File))
                        gArg_File = arg;
                    else
                        argv.Add(arg);

                    lastOption = "";
                    getArgument = false;
                }
            }

            if (getArgument)
            {
                Console.Error.WriteLine("Error: missing value for option '{0}'", lastOption);
                return false;
            }

            gArgv = argv.ToArray();

            return true;
        }

        static int Main(string[] args)
        {
            if (!ParseArguments(args))
            {
                if (gArg_Help)
                    ShowUsage();
                return 1;
            }

            if (string.IsNullOrEmpty(gArg_Step) || gArg_Step.Contains("stepA"))
            {
                var step = new StepA();
                step.Repl();
            }
            else if (gArg_Step.Contains("step9"))
            {
                var step = new Step9();
                step.Repl();
            }
            else if (gArg_Step.Contains("step8"))
            {
                var step = new Step8();
                step.Repl();
            }
            else if (gArg_Step.Contains("step7"))
            {
                var step = new Step7();
                step.Repl();
            }
            else if (gArg_Step.Contains("step6"))
            {
                var step = new Step6();
                step.Repl();
            }
            else if (gArg_Step.Contains("step5"))
            {
                var step = new Step5();
                step.Repl();
            }
            else if (gArg_Step.Contains("step4"))
            {
                var step = new Step4();
                step.Repl();
            }
            else if (gArg_Step.Contains("step3"))
            {
                var step = new Step3();
                step.Repl();
            }
            else if (gArg_Step.Contains("step2"))
            {
                var step = new Step2();
                step.Repl();
            }
            else if (gArg_Step.Contains("step1"))
            {
                var step = new Step1();
                step.Repl();
            }
            else if (gArg_Step.Contains("step0"))
            {
                var step = new Step0();
                step.Repl();
            }
            else
            {
                Console.Error.WriteLine("ERROR: unknown step!");
                return 1;
            }

            return 0;
        }
    }
}
