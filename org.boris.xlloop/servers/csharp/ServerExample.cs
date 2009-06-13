using System;
using System.IO;
using System.Collections.Generic;
using XLLoop;

static class Server
{
    class TestHandler : FunctionHandler
    {
        private static char[] Characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789".ToCharArray();

        public XLoper Execute(String name, XLoper[] args)
        {
            Console.WriteLine(name);
            XLoper x = new XLoper();
            x.Type = XLoper.xlTypeStr;
            x.Str = "#Unknown Function";

            if (name.Equals("RandTest"))
            {
                x.Type = XLoper.xlTypeMulti;
                x.Arr.Arr = new XLoper[new Random().Next(50) + 2];
                x.Arr.Rows = (uint)x.Arr.Arr.Length;
                x.Arr.Columns = 1;
                Random r = new Random();
                for (int i = 0; i < x.Arr.Arr.Length; i++)
                {
                    x.Arr.Arr[i] = MakeRandom(r);
                }
            }
            else if (name.Equals("ArrayTest"))
            {
            }

            return x;
        }

        private XLoper MakeRandom(Random r)
        {
            XLoper x = new XLoper();
            x.Type = XLoper.xlTypeNil;
            int choice = r.Next(7);
            switch (choice)
            {
                case 0:
                    x.Type = XLoper.xlTypeStr;
                    x.Str = MakeRandomString(r.Next(1000));
                    break;
                case 1:
                    x.Type = XLoper.xlTypeNum;
                    x.Num = r.NextDouble() * 1000;
                    break;
                case 2:
                    x.Type = XLoper.xlTypeInt;
                    x.W = r.Next(1000);
                    break;
                case 3:
                    x.Type = XLoper.xlTypeBool;
                    x.Bool = r.Next(2) == 1;
                    break;
                case 4:
                    x.Type = XLoper.xlTypeStr;
                    x.Str = "";
                    break;
            }

            return x;
        }

        private static String MakeRandomString(int len)
        {
            char[] c = new char[len];
            int cl = Characters.Length;
            Random r = new Random();
            for (int i = 0; i < len; i++)
            {
                c[i] = Characters[r.Next(cl)];
            }
            return new String(c);
        }
    }

    static void Main()
    {
        System.Console.WriteLine("Testing...");
        TestHandler t = new TestHandler();
        FunctionServer f = new FunctionServer(t);
        f.Run();
    }
}
