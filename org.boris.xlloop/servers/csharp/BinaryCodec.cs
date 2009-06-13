using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace XLLoop
{
    class BinaryCodec
    {
        public static XLoper Decode(Stream s)
        {
            int type = (int)s.ReadByte();
            switch (type)
            {
                case XLoper.xlTypeBool:
                    return DecodeBool(s);
                case XLoper.xlTypeErr:
                    return DecodeError(s);
                case XLoper.xlTypeInt:
                    return DecodeInt(s);
                case XLoper.xlTypeMissing:
                    return DecodeMissing();
                case XLoper.xlTypeMulti:
                    return DecodeMulti(s);
                case XLoper.xlTypeNil:
                    return DecodeNil();
                case XLoper.xlTypeNum:
                    return DecodeNum(s);
                case XLoper.xlTypeStr:
                    return DecodeStr(s);
                case -1:
                    throw new EndOfStreamException();
                default:
                    throw new IOException("Invalid type encountered: " + type);
            }
        }

        private static XLoper DecodeStr(Stream s)
        {
            int len = s.ReadByte();
            if (len == -1)
                throw new EndOfStreamException();
            byte[] b = new byte[len];
            int r = s.Read(b, 0, len);
            if (r == -1)
                throw new EndOfStreamException();
            while (r < len)
            {
                int sf = s.Read(b, r, len - r);
                if (sf == -1)
                    throw new EndOfStreamException();
                r += sf;
            }
            XLoper x = new XLoper();
            x.Type = XLoper.xlTypeStr;
            x.Str = Encoding.UTF8.GetString(b);
            return x;
        }

        private static XLoper DecodeNum(Stream s)
        {
            XLoper x = new XLoper();
            x.Type = XLoper.xlTypeNum;
            x.Num = BitConverter.Int64BitsToDouble(((long)ReadDWORD(s) << 32)
                    | (long)ReadDWORD(s));
            return x;
        }

        private static XLoper DecodeNil()
        {
            XLoper x = new XLoper();
            x.Type = XLoper.xlTypeNil;
            return x;
        }

        private static XLoper DecodeMulti(Stream s)
        {
            XLoper x = new XLoper();
            x.Type = XLoper.xlTypeMulti;
            x.Arr = new multi();
            x.Arr.Rows = ReadDWORD(s);
            x.Arr.Columns = ReadDWORD(s);
            uint len = x.Arr.Rows * x.Arr.Columns;
            x.Arr.Arr = new XLoper[len];
            for (uint i = 0; i < len; i++)
            {
                x.Arr.Arr[i] = Decode(s);
            }
            return x;
        }

        private static XLoper DecodeMissing()
        {
            XLoper x = new XLoper();
            x.Type = XLoper.xlTypeMissing;
            return x;
        }

        private static XLoper DecodeInt(Stream s)
        {
            XLoper x = new XLoper();
            x.Type = XLoper.xlTypeInt;
            x.W = (int)ReadDWORD(s);
            return x;
        }

        private static XLoper DecodeError(Stream s)
        {
            XLoper x = new XLoper();
            x.Type = XLoper.xlTypeErr;
            x.W = (int)ReadDWORD(s);
            return x;
        }

        private static XLoper DecodeBool(Stream s)
        {
            XLoper x = new XLoper();
            x.Type = XLoper.xlTypeBool;
            x.Bool = s.ReadByte() != 0;
            return x;
        }

        public static void Encode(XLoper x, Stream s)
        {
            s.WriteByte((byte) x.Type);
            switch (x.Type)
            {
                case XLoper.xlTypeBool:
                    EncodeBoolean(x, s);
                    break;
                case XLoper.xlTypeErr:
                    EncodeError(x, s);
                    break;
                case XLoper.xlTypeInt:
                    EncodeInt(x, s);
                    break;
                case XLoper.xlTypeMissing:
                    break;
                case XLoper.xlTypeMulti:
                    EncodeMulti(x, s);
                    break;
                case XLoper.xlTypeNil:
                    break;
                case XLoper.xlTypeNum:
                    EncodeNum(x, s);
                    break;
                case XLoper.xlTypeStr:
                    EncodeStr(x, s);
                    break;
                default:
                    throw new IOException("Invalid XLoper type encountered: " + x.Type);
            }
        }

        private static void EncodeStr(XLoper x, Stream s)
        {
            byte[] b = Encoding.UTF8.GetBytes(x.Str);
            if (b.Length > 255)
            {
                s.WriteByte((byte)255);
                s.Write(b, 0, 255);
            }
            else
            {
                s.WriteByte((byte)b.Length);
                s.Write(b, 0, b.Length);
            }
        }

        private static void EncodeNum(XLoper x, Stream s)
        {
            long l = BitConverter.DoubleToInt64Bits(x.Num);
            WriteDWORD((uint)(l >> 32), s);
            WriteDWORD((uint)l, s);
        }

        private static void EncodeMulti(XLoper x, Stream s)
        {
            WriteDWORD(x.Arr.Rows, s);
            WriteDWORD(x.Arr.Columns, s);
            for (int i = 0; i < x.Arr.Arr.Length; i++)
            {
                Encode(x.Arr.Arr[i], s);
            }
        }

        private static void EncodeInt(XLoper x, Stream s)
        {
            WriteDWORD((uint) x.W, s);
        }

        private static void EncodeError(XLoper x, Stream s)
        {
            WriteDWORD((uint)x.Err, s);
        }

        private static void EncodeBoolean(XLoper x, Stream s)
        {
            s.WriteByte((byte) (x.Bool ? 1 : 0));
        }

        private static void WriteDWORD(uint v, Stream w)
        {
            w.WriteByte((byte)(v >> 24 & 0xff));
            w.WriteByte((byte)(v >> 16 & 0xff));
            w.WriteByte((byte)(v >> 8 & 0xff));
            w.WriteByte((byte)(v & 0xff));
        }

        private static uint ReadDWORD(Stream s)
        {
            return ((uint)s.ReadByte() << 24) | ((uint)s.ReadByte() << 16) | ((uint)s.ReadByte() << 8)
                    | (uint)s.ReadByte();
        }

    }
}
