using System;
using System.Collections.Generic;
using System.Numerics;

namespace Polinoms
{
    delegate cpolinom Inverse(cpolinom p, cpolinom ir);
    delegate cpolinom Multi(cpolinom a, cpolinom b);

    abstract class Pow
    {
        private static List<cpolinom> SlideRLTable(cpolinom p, cpolinom ir, Multi mul, int power, int w)
        {
            List<cpolinom> table = new List<cpolinom>();
            table.Add(BinaryRL(p, ir, mul, power));
            for (int i = 0; i < power - 1; i++)
                table.Add(mul(table[i], p) % ir);
            return table;
        }
        private static List<cpolinom> SlideLRTable(cpolinom p, cpolinom ir, Multi mul, int power, int w)
        {
            List<cpolinom> table = new List<cpolinom>();
            table.Add(BinaryLR(p, ir, mul, power));
            for (int i = 0; i < power - 1; i++)
                table.Add(mul(table[i], p) % ir);
            return table;
        }
        private static List<cpolinom> NAFRLTable(cpolinom p, cpolinom ir, Multi mul, int power, int w)
        {
            List<cpolinom> table = new List<cpolinom>();

            for (int i = 1; i <= power; i += 2)
                table.Add(BinaryRL(p, ir, mul, i));
            return table;
        }
        private static List<cpolinom> NAFLRTable(cpolinom p, cpolinom ir, Multi mul, int power, int w)
        {
            List<cpolinom> table = new List<cpolinom>();

            for (int i = 1; i <= power; i += 2)
                table.Add(BinaryLR(p, ir, mul, i));
            return table;
        }
        private static List<cpolinom> Table(cpolinom p, cpolinom ir, Multi mul, int w)
        {
            List<cpolinom> table = new List<cpolinom>();
            table.Add(p % ir);
            for (int i = 0; i < Math.Pow(2, w) - 2; i++)
                table.Add(mul(table[i], p) % ir);
            return table;
        }
        private static List<string> windows(string s, int w)
        {
            string st = s;

            while (st.Length % w != 0)
                st = "0" + st;


            List<string> bins = new List<string>();
            for (int i = 0; i < st.Length / w; i++)
                bins.Add(st.Substring(i * w, w));

            return bins;
        }
        private static List<BigInteger> FindLargest1(List<BigInteger> x, int i, int w)
        {
            int j = i;
            int pow = 1;
            BigInteger temp = x[i];

            int max_j = i;
            BigInteger max = x[i];
            while (j - i + 1 < w && j < x.Count - 1)
            {
                j++;

                pow = pow * 2;
                temp = temp + pow * x[j];

                if (temp % 2 != 0)
                {
                    max_j = j;
                    max = temp;
                }

            }
            max_j = max_j - i + 1;

            List<BigInteger> r = new List<BigInteger>();
            r.Add(max);
            r.Add(max_j);
            return r;
        }
        private static List<BigInteger> FindLargest2(List<BigInteger> x, int i, int w)
        {
            int j = i;
            int pow = 1;
            BigInteger temp = x[i];
            int max_j = i;
            BigInteger max = x[i];
            while (i - j + 1 < w && j > 0)
            {
                pow = 1;
                temp = 0;
                j--;
                for (int t = j; t <= i; t++)
                {
                    temp = temp + pow * x[t];
                    pow = pow * 2;
                }

                if (temp % 2 != 0)
                {
                    max_j = j;
                    max = temp;
                }
            }
            max_j = i - max_j + 1;

            List<BigInteger> r = new List<BigInteger>();
            r.Add(max);
            r.Add(max_j);
            return r;
        }
        private static List<int[]> ToDBNS2RL(BigInteger n)
        {
            List<int[]> x = new List<int[]>();
            BigInteger v = n;
            while (v != 0)
            {
                x.Insert(0, new int[3]);
                int a = 0;
                while (v % 2 == 0 && v != 0)
                {
                    a++;
                    v /= 2;
                }
                int b = 0;
                while (v % 3 == 0 && v != 0)
                {
                    b++;
                    v /= 3;
                }
                x[0][1] = a;
                x[0][2] = b;

                v--;
                if (v != 0)
                {
                    if (v % 6 != 0)
                    {
                        v += 2;
                        x[0][0] = -1;
                    }
                    else
                        x[0][0] = 1;
                }
                else
                    x[0][0] = 1;
            }
            return x;
        }
        private static List<int[]> ToDBNS2LR(BigInteger n)
        {
            List<int[]> x = new List<int[]>();
            x.Add(new int[3]);
            int i = 0;
            BigInteger v = n;
            x[i][0] = 0;
            while (v != 0)
            {
                int a = 0;
                while (v % 2 == 0 && v != 0)
                {
                    a++;
                    v = v / 2;
                }

                int b = 0;
                while (v % 3 == 0 && v != 0)
                {
                    b++;
                    v = v / 3;
                }
                x[i][1] = a;
                x[i][2] = b;
                i++;
                x.Add(new int[3]);
                v--;
                if (v != 0)
                {
                    if (v % 6 != 0)
                    {
                        v = v + 2;
                        x[i][0] = -1;
                    }
                    else
                        x[i][0] = 1;
                }
            }
            return x;
        }

        public static cpolinom BinaryRL(cpolinom p, cpolinom ir, Multi mul, BigInteger n)
        {
            cpolinom res = new cpolinom("1", p.mod);
            cpolinom t = cpolinom.Copy(p);
            string Binary = bif.ToBin(n);
            for (int i = Binary.Length - 1; i >= 0; i--)
            {
                if (Binary[i] == '1')
                    res = mul(t, res) % ir;
                t = mul(t, t) % ir;
            }
            return res;
        }
        public static cpolinom BinaryLR(cpolinom p, cpolinom ir, Multi mul, BigInteger n)
        {
            cpolinom res = new cpolinom("1", p.mod);
            cpolinom t = cpolinom.Copy(p);
            string Binary = bif.ToBin(n);
            for (int i = 0; i < Binary.Length; i++)
            {
                res = mul(res, res) % ir;
                if (Binary[i] == '1')
                    res = mul(t, res) % ir;
            }
            return res;
        }

        public static cpolinom WindowRL(cpolinom p, cpolinom ir, Multi mul, BigInteger n, int w)
        {
            List<cpolinom> table = Table(p, ir, mul, w);
            cpolinom res = new cpolinom("1", p.mod);

            List<string> bins = windows(bif.ToBin(n), w);

            for (int i = bins.Count - 1; i > -1; i--)
            {
                int c = Convert.ToInt32(bins[i], 2);
                if (c != 0) res = mul(res, table[c - 1]) % ir;

                for (int k = 0; k < w; k++)
                    for (int j = 0; j < table.Count; j++)
                        table[j] = mul(table[j], table[j])%ir;
            }
            return res;
        }
        public static cpolinom WindowLR(cpolinom p, cpolinom ir, Multi mul, BigInteger n, int w)
        {
            List<cpolinom> table = Table(p, ir, mul, w);
            cpolinom res = new cpolinom("1", p.mod);

            List<string> bins = windows(bif.ToBin(n), w);

            for (int i = 0; i < bins.Count; i++)
            {
                for (int k = 0; k < w; k++)
                    res = mul(res, res) % ir;

                int c = Convert.ToInt32(bins[i], 2);
                if (c != 0) res = mul(res, table[c - 1]) % ir;
            }
            return res;
        }

        public static cpolinom SlideRL(cpolinom p, cpolinom ir, Multi mul, BigInteger n, int w)
        {
            int power = (int)Math.Pow(2, w - 1);
            List<cpolinom> table = SlideRLTable(p, ir, mul, power, w);
            
            cpolinom res = new cpolinom("1", p.mod);
            cpolinom temp = cpolinom.Copy(p);

            string binary = bif.ToBin(n);

            while(binary.Length > 0)
            {
                int index = binary.Length - 1;
                if (binary.Length < w || binary[index - w + 1] == '0')
                {
                    if (binary[index] == '1')
                        res = mul(res, temp) % ir;

                    for (int j = 0; j < table.Count; j++)
                        table[j] = mul(table[j], table[j]) % ir;
                    
                    temp = mul(temp, temp) % ir;

                    binary = binary.Remove(index, 1);
                }
                else
                {
                    int c = Convert.ToInt32(binary.Substring(index - w + 1, w), 2);
                    res = mul(res, table[c - power]) % ir;

                    temp = mul(temp, table[table.Count - 1]) % ir;

                    for (int k = 0; k < w; k++)
                        for (int j = 0; j < table.Count; j++)
                            table[j] = mul(table[j], table[j]) % ir;

                    binary = binary.Remove(index - w + 1, w);
                }
            }
            return res;
        }
        public static cpolinom SlideLR(cpolinom p, cpolinom ir, Multi mul, BigInteger n, int w)
        {
            int power = (int)Math.Pow(2, w - 1);
            List<cpolinom> table = SlideLRTable(p, ir, mul, power, w);

            cpolinom res = new cpolinom("1", p.mod);
            string binary = bif.ToBin(n);

            while (binary.Length > 0)
            {
                if (binary.Length < w || binary[0] == '0')
                {
                    res = mul(res, res) % ir;
                    if (binary[0] == '1')
                        res = mul(res, p) % ir;
                    binary = binary.Remove(0, 1);
                }
                else
                {
                    int c = Convert.ToInt32(binary.Substring(0, w), 2);

                    for (int k = 0; k < w; k++)
                        res = mul(res, res) % ir;

                    res = mul(res, table[c - power]) % ir;
                    binary = binary.Remove(0, w);
                }
            }
            return res;
        }

        public static cpolinom NAFBinaryRL(cpolinom p, cpolinom ir, Multi mul, BigInteger n, Inverse inv)
        {
            cpolinom res = new cpolinom("1", p.mod);
            cpolinom c = cpolinom.Copy(p);
            List<BigInteger> x = bif.NAF(n);
            for (int i = 0; i < x.Count; i++)            
            {
                if (x[i] == 1) res = mul(res, c) % ir;
                else if (x[i] == -1) res = mul(res, inv(c, ir)) % ir;
                c = mul(c, c) % ir;
            }
            return res;
        }
        public static cpolinom NAFBinaryLR(cpolinom p, cpolinom ir, Multi mul, BigInteger n, Inverse inv)
        {
            cpolinom res = new cpolinom("1", p.mod);
            cpolinom c = cpolinom.Copy(p);
            List<BigInteger> x = bif.NAF(n);
            for (int i = x.Count - 1; i > -1; i--)
            {
                res = mul(res, res) % ir;
                if (x[i] == 1) res = mul(res, c) % ir;
                else if (x[i] == -1) res = mul(res, inv(c, ir)) % ir;
            }
            return res;
        }
        public static cpolinom meth7_2(cpolinom p, cpolinom ir, Multi mul, BigInteger n, Inverse inv)
        {
            cpolinom res = new cpolinom("1", p.mod);
            cpolinom c = cpolinom.Copy(p);
            BigInteger k = n;
            while (k >= 1)
            {
                BigInteger temp = 0;
                if (k % 2 != 0)
                {
                    temp = 2 - k % 4;
                    k -= temp;
                }

                if (temp == 1) res = mul(res, c) % ir;
                else if (temp == -1) res = mul(res, inv(c, ir)) % ir;

                k = k / 2;
                c = mul(c, c) % ir;

            }
            return res;
        }

        public static cpolinom NAFSlideRL(cpolinom p, cpolinom ir, Multi mul, BigInteger n, int w, Inverse inv)
        {
            cpolinom res = new cpolinom("1", p.mod);
            List<BigInteger> x = bif.NAF(n);
            int pow = 2 * ((int)Math.Pow(2, w) - (int)Math.Pow((-1), w)) / 3 - 1;
            List<cpolinom> table = NAFRLTable(p, ir, mul, pow, w);

            for (int i = 0; i < x.Count; )
            {
                List<BigInteger> max = FindLargest1(x, i, w);

                if (max[0] > 0)
                    res = mul(res, table[(int)(bif.Abs(max[0]) / 2)]) % ir;
                else if (max[0] < 0)
                    res = mul(res, inv(table[(int)(bif.Abs(max[0]) / 2)], ir)) % ir;

                for (int d = 0; d < max[1]; d++)
                    for (int j = 0; j < table.Count; j++)
                        table[j] = mul(table[j], table[j]) % ir;

                i = i + (int)max[1];
            }
            return res;
        }
        public static cpolinom NAFSlideLR(cpolinom p, cpolinom ir, Multi mul, BigInteger n, int w, Inverse inv)
        {
            cpolinom res = new cpolinom("1", p.mod);
            List<BigInteger> x = bif.NAF(n);
            int pow = 2 * ((int)Math.Pow(2, w) - (int)Math.Pow((-1), w)) / 3 - 1;
            List<cpolinom> table = NAFLRTable(p, ir, mul, pow, w);

            for (int i = x.Count - 1; i > -1; )
            {
                List<BigInteger> max = new List<BigInteger>();
                if (x[i] == 0)
                {
                    max.Add(0);
                    max.Add(1);
                }
                else
                    max = FindLargest2(x, i, w);

                for (int d = 0; d < max[1]; d++)
                    res = mul(res, res) % ir;

                if (max[0] > 0)
                    res = mul(res, table[(int)(bif.Abs(max[0]) / 2)]) % ir;
                else if (max[0] < 0)
                    res = mul(res, inv(table[(int)(bif.Abs(max[0]) / 2)], ir)) % ir;

                i = i - (int)max[1];
            }
            return res;
        }

        public static cpolinom NAFWindowRL(cpolinom p, cpolinom ir, Multi mul, BigInteger n, int w, Inverse inv)
        {
            cpolinom res = new cpolinom("1", p.mod);
            List<BigInteger> x = bif.NAF(n);
            int pow = (int)Math.Pow(2, w - 1);
            List<cpolinom> table = NAFRLTable(p, ir, mul, pow, w);            

            for (int i = 1; i < pow; i += 2)
                table.Add(BinaryRL(p, ir, mul, i));

            for (int i = 0; i < x.Count; i++)
            {
                if (x[i] > 0)
                    res = mul(res, table[(int)(x[i] / 2)]) % ir;
                else if (x[i] < 0)
                    res = mul(res, inv(table[(int)(-x[i] / 2)], ir)) % ir;
                
                for (int j = 0; j < table.Count; j++)
                    table[j] = mul(table[j], table[j]) % ir;
            }
            return res;
        }
        public static cpolinom NAFWindowLR(cpolinom p, cpolinom ir, Multi mul, BigInteger n, int w, Inverse inv)
        {
            cpolinom res = new cpolinom("1", p.mod);
            List<BigInteger> x = bif.NAF(n);
            int pow = (int)Math.Pow(2, w - 1);
            List<cpolinom> table = NAFLRTable(p, ir, mul, pow, w);

            for (int i = x.Count - 1; i > -1 ; i--)
            {
                res = mul(res, res) % ir;
                if (x[i] > 0)
                    res = mul(res, table[(int)(x[i] / 2)]) % ir;
                else if (x[i] < 0)
                    res = mul(res, inv(table[(int)(-x[i] / 2)], ir)) % ir;
            }
            return res;
        }

        public static cpolinom wNAFSlideRL(cpolinom p, cpolinom ir, Multi mul, BigInteger n, int w, Inverse inv)
        {
            cpolinom res = new cpolinom("1", p.mod);
            List<BigInteger> x = bif.wNAF(n, w);
            int pow = 2 * ((int)Math.Pow(2, w) - (int)Math.Pow((-1), w)) / 3 - 1;

            List<cpolinom> table = NAFRLTable(p, ir, mul, pow, w);

            for (int i = x.Count - 1; i > -1; )
            {
                List<BigInteger> max = FindLargest1(x, i, w);

                if (max[0] > 0)
                    res = mul(res, table[(int)(bif.Abs(x[i]) / 2)]) % ir;
                else if (max[0] < 0)
                    res = mul(res, inv(table[(int)(bif.Abs(x[i]) / 2)], ir)) % ir;

                for (int d = 0; d < max[1]; d++)
                    for (int j = 0; j < table.Count; j++)
                        table[j] = mul(table[j], table[j]) % ir;

                i = i - (int)max[1];
            }
            return res;
        }
        public static cpolinom wNAFSlideLR(cpolinom p, cpolinom ir, Multi mul, BigInteger n, int w, Inverse inv)
        {
            cpolinom res = new cpolinom("1", p.mod);
            List<BigInteger> x = bif.wNAF(n, w);
            int pow = 2 * ((int)Math.Pow(2, w) - (int)Math.Pow((-1), w)) / 3 - 1;

            List<cpolinom> table = NAFLRTable(p, ir, mul, pow, w);

            for (int i = 0; i < x.Count; )
            {
                List<BigInteger> max = new List<BigInteger>();
                if (x[i] == 0)
                {
                    max.Add(0);
                    max.Add(1);
                }
                else
                    max = FindLargest2(x, i, w);

                for (int d = 0; d < max[1]; d++)
                    res = mul(res, res) % ir;

                if (max[0] > 0)
                    res = mul(res, table[(int)(bif.Abs(x[i]) / 2)]) % ir;
                else if (max[0] < 0)
                    res = mul(res, inv(table[(int)(bif.Abs(x[i]) / 2)], ir)) % ir;

                i = i + (int)max[1];
            }
            return res;
        }

        public static cpolinom AddSubRL(cpolinom p, cpolinom ir, Multi mul, BigInteger n, Inverse inv)
        {
            cpolinom res = new cpolinom("1", p.mod);
            cpolinom c = cpolinom.Copy(p);

            string BinaryT = bif.ToBin(3 * n);
            string BinaryN = bif.ToBin(n);
            while (BinaryN.Length < BinaryT.Length) BinaryN = "0" + BinaryN;

            for (int i = BinaryN.Length - 2; i >= 0 ; i--)
            {
                if (BinaryT[i] != '0' && BinaryN[i] == '0') res = mul(res, c) % ir;
                else if (BinaryT[i] == '0' && BinaryN[i] != '0') res = mul(res, inv(c, ir)) % ir;
                c = mul(c, c) % ir;
            }

            return res;
        }
        public static cpolinom AddSubLR(cpolinom p, cpolinom ir, Multi mul, BigInteger n, Inverse inv)
        {
            cpolinom res = new cpolinom("1", p.mod);
            cpolinom c = cpolinom.Copy(p);

            string BinaryT = bif.ToBin(3 * n);
            string BinaryN = bif.ToBin(n);
            while (BinaryN.Length < BinaryT.Length) BinaryN = "0" + BinaryN;

            for (int i = 0; i < BinaryN.Length - 1; i++)
            {
                res = mul(res, res) % ir;
                if (BinaryT[i] != '0' && BinaryN[i] == '0') res = mul(res, c) % ir;
                else if (BinaryT[i] == '0' && BinaryN[i] != '0') res = mul(res, inv(c, ir)) % ir;
            }
            
            return res;
        }

        public static cpolinom Joye_double_and_add(cpolinom p, cpolinom ir, Multi mul, BigInteger n)
        {
            cpolinom res = new cpolinom("1", p.mod);
            cpolinom t = cpolinom.Copy(p);

            string Binary = bif.ToBin(n);
            for (int i = Binary.Length - 1; i >= 0 ; i--)
            {
                if (Binary[i] == '1')
                {
                    res = mul(res, res) % ir;
                    res = mul(res, t) % ir;
                }
                else
                {
                    t = mul(t, t) % ir;
                    t = mul(res, t) % ir;
                }
            }
            return res;
        }
        public static cpolinom MontgomeryLadder(cpolinom p, cpolinom ir, Multi mul, BigInteger n)
        {
            cpolinom res = new cpolinom("1", p.mod);
            cpolinom t = cpolinom.Copy(p);

            string Binary = bif.ToBin(n);
            for (int i = 0; i < Binary.Length; i++)
            {
                if (Binary[i] == '0')
                {
                    t = mul(t, res) % ir;
                    res = mul(res, res) % ir;
                }
                else
                {
                    res = mul(res, t) % ir;
                    t = mul(t, t) % ir;
                }
            }
            return res;
        }

        public static cpolinom DBNS1RL(cpolinom p, cpolinom ir, Multi mul, BigInteger n, int A, int B, Inverse inv)
        {
            return methods.Point_Multiplication_Affine_Coord_19(p, ir, mul, n, A, B, inv);
        }
        public static cpolinom DBNS1LR(cpolinom p, cpolinom ir, Multi mul, BigInteger n, int A, int B, Inverse inv)
        {
            return methods.Point_Multiplication_Affine_Coord_20(p, ir, mul, n, A, B, inv);
        }

        public static cpolinom DBNS2RL(cpolinom p, cpolinom ir, Multi mul, BigInteger n, Inverse inv)
        {
            List<int[]> x = ToDBNS2RL(n);

            cpolinom res = new cpolinom("1", p.mod);
            cpolinom t = cpolinom.Copy(p);
            for (int i = x.Count - 1; i > -1; i--)
            {
                for (int j = 0; j < x[i][1]; j++)
                    t = mul(t, t) % ir;
                for (int j = 0; j < x[i][2]; j++)
                    t = mul(t, mul(t, t)) % ir;

                if (x[i][0] == 1)
                    res = mul(res, t) % ir;
                else if (x[i][0] == -1)
                    res = mul(res, inv(t, ir)) % ir;              
            }
            return res;
        }
        public static cpolinom DBNS2LR(cpolinom p, cpolinom ir, Multi mul, BigInteger n, Inverse inv)
        {
            List<int[]> x = ToDBNS2LR(n);
            cpolinom res = cpolinom.Copy(p);

            for (int i = x.Count - 1; i > 0 ; i--)
            {
                for (int j = 0; j < x[i][1]; j++)
                    res = mul(res, res) % ir;
                for (int j = 0; j < x[i][2]; j++)
                    res = mul(res, mul(res, res)) % ir;

                if (x[i][0] == 1)
                    res = mul(res, p) % ir;
                else if (x[i][0] == -1)
                    res = mul(res, inv(p, ir)) % ir;
            }
            for (int j = 0; j < x[0][1]; j++)
                res = mul(res, res) % ir;
            for (int j = 0; j < x[0][2]; j++)
                res = mul(res, mul(res, res)) % ir;
            return res;
        }
    }
    
}
