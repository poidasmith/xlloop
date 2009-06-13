using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Threading;

namespace XLLoop
{
    class FunctionServer
    {
        private FunctionHandler handler;
        private int port;
        private TcpListener listener;

        public FunctionServer(FunctionHandler handler) : this(handler, 5454)
        {
        }

        public FunctionServer(FunctionHandler handler, int port)
        {
            this.handler = handler;
            this.port = port;
        }

        public void Run()
        {
            if (listener == null)
            {
                IPAddress local = new IPAddress(new byte[] { 127, 0, 0, 1 });
                listener = new TcpListener(local, port);
                listener.Start();
            }

            while (true)
            {
                Handler h = new Handler(handler, listener.AcceptSocket());
                ThreadStart ts = new ThreadStart(h.Run);
                Thread t = new Thread(ts);
                t.IsBackground = true;
                t.Start();
            }
        }

        public void Start()
        {
            ThreadStart ts = new ThreadStart(Run);
            Thread t = new Thread(ts);
            t.Name = "XLLoop Function Server";
            t.IsBackground = true;
            t.Start();
        }

        public void Stop()
        {
            listener.Stop();
        }
    }

    class Handler
    {
        private FunctionHandler handler;
        private Socket socket;
        private Stream stream;

        public Handler(FunctionHandler handler, Socket socket)
        {
            this.handler = handler;
            this.socket = socket;
            this.stream = new NetworkStream(socket);
        }

        public void Run()
        {
            while (socket.IsBound)
            {
                try
                {
                    XLoper name = BinaryCodec.Decode(stream);
                    if (name.Type != XLoper.xlTypeStr)
                    {
                        throw new Exception("Protocol Error: expecting function name");
                    }
                    XLoper argc = BinaryCodec.Decode(stream);
                    if (argc.Type != XLoper.xlTypeInt)
                    {
                        throw new Exception("Protocol Error: expecting arg count");
                    }
                    XLoper[] args = new XLoper[argc.W];
                    for (int i = 0; i < argc.W; i++)
                    {
                        args[i] = BinaryCodec.Decode(stream);
                    }

                    try
                    {
                        XLoper res = handler.Execute(name.Str, args);
                        BinaryCodec.Encode(res, stream);
                    }
                    catch (RequestException re)
                    {
                        XLoper x = new XLoper();
                        x.Type = XLoper.xlTypeStr;
                        x.Str = re.Message;
                        BinaryCodec.Encode(x, stream);
                    }
                }
                catch (Exception e)
                {
                    Console.WriteLine(e.StackTrace);
                    socket.Close();
                }
            }
        }
    }
}
