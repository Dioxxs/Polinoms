using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Numerics;

namespace Polinoms
{
    class methods
    {

        public static cpolinom Point_Multiplication_Affine_Coord_19(cpolinom p, cpolinom ir, Multi mul, BigInteger n, int A, int B, Inverse inv)
        {
            BigInteger[,] mas_k;
            mas_k = Convert_to_DBNS_1(n, A, B);

            int lastindex = mas_k.GetLength(0) - 1;
            cpolinom t = cpolinom.Copy(p);
            cpolinom res = new cpolinom("1", p.mod);

            for (int i = 0; i < mas_k[lastindex, 1]; i++)
                t = mul(t, t) % ir;


            for (int i = 0; i < mas_k[lastindex, 2]; i++)
                t = mul(t, mul(t, t)) % ir;

            if (mas_k[lastindex, 0] == -1)
                res = mul(res, inv(t, ir)) % ir;
            else if (mas_k[lastindex, 0] == 1)
                res = mul(res, t) % ir;

            for (int i = lastindex - 1; i >= 0; i--)
            {
                BigInteger u = mas_k[i, 1] - mas_k[i + 1, 1];
                BigInteger v = mas_k[i, 2] - mas_k[i + 1, 2];
                for (int j = 0; j < u; j++)                
                    t = mul(t, t) % ir;                

                for (int j = 0; j < v; j++)                
                    t = mul(t, mul(t, t)) % ir;

                if (mas_k[i, 0] == -1)
                    res = mul(res, inv(t, ir)) % ir;
                else if (mas_k[i, 0] == 1)
                    res = mul(res, t) % ir;
            }
            return res;
        }

        public static cpolinom Point_Multiplication_Affine_Coord_20(cpolinom p, cpolinom ir, Multi mul, BigInteger n, int A, int B, Inverse inv)
        {
            BigInteger[,] mas_k;

            cpolinom t = cpolinom.Copy(p);
            cpolinom res = new cpolinom("1", p.mod);

            mas_k = Convert_to_DBNS_2(n, A, B);

            if (mas_k[0, 0] == -1)
                res = inv(t, ir);
            else if (mas_k[0, 0] == 1)
                res = t;

            for (int i = 0; i < mas_k.GetLength(0) - 1; i++)
            {
                BigInteger u = mas_k[i, 1] - mas_k[i + 1, 1];
                BigInteger v = mas_k[i, 2] - mas_k[i + 1, 2];
                for (int j = 0; j < u; j++)                
                    res = mul(res, res) % ir;

                for (int j = 0; j < v; j++)
                    res = mul(res, mul(res, res)) % ir;

                if (mas_k[i+1, 0] < 0)
                    res = mul(res, inv(t, ir)) % ir;
                else
                    res = mul(res, t) % ir;     
            }

            for (int i = 0; i < mas_k[mas_k.GetLength(0) - 1, 1]; i++)
                res = mul(res, res) % ir;

            for (int i = 0; i < mas_k[mas_k.GetLength(0) - 1, 2]; i++)
                res = mul(res, mul(res, res)) % ir;
            return res;
        }

        public static BigInteger[,] Convert_to_DBNS_1(BigInteger k, int a_max, int b_max)
        {
            List<BigInteger[]> mass_k_l = new List<BigInteger[]>();
            int s = 1;
            List<int> app;
            int a = a_max;
            int b = b_max;
            while (k > 0)
            {
                app = Best_Approximation_1(k, a, b);
                a = app[0];
                b = app[1];
                BigInteger z = bif.Pow(2, a) * bif.Pow(3, b);
                mass_k_l.Add(new BigInteger[3] { s, a, b });
                if (k < z)                
                    s = -s;
                
                k = bif.Abs(k - z);
            }
            BigInteger[,] mass_k = new BigInteger[mass_k_l.Count, 3];
            for (int j = 0; j < mass_k_l.Count; j++)
            {
                mass_k[j, 0] = mass_k_l[j][0];
                mass_k[j, 1] = mass_k_l[j][1];
                mass_k[j, 2] = mass_k_l[j][2];
            }
            return mass_k;
        }

        public static BigInteger[,] Convert_to_DBNS_2(BigInteger k, int a_max, int b_max)
        {
            List<BigInteger[]> mass_k_l = new List<BigInteger[]>();
            int i = 0;
            int s = 1;
            List<int> app;
            int a = a_max;
            int b = b_max;
            while (k > 0)
            {
                i++;
                app = Best_Approximation_2(k, a, b);
                a = app[0];
                b = app[1];
                int z = (int)(Math.Pow(2, (double)a) * Math.Pow(3, (double)b));
                mass_k_l.Add(new BigInteger[3] { s, a, b });
                if (k < z)                
                    s = -s;
                
                k = bif.Abs(k - z);
            }

            BigInteger[,] mass_k = new BigInteger[mass_k_l.Count, 3];
            for (int j = 0; j < mass_k_l.Count; j++)
            {
                mass_k[j, 0] = mass_k_l[j][0];
                mass_k[j, 1] = mass_k_l[j][1];
                mass_k[j, 2] = mass_k_l[j][2];
            }
            return mass_k;
        }

        public static List<int> Best_Approximation_1(BigInteger k, int a_max, int b_max)
        {
            int a;
            int b;
            int min_x = a_max;
            int y = (int)Math.Round((double)(-min_x) * log_dif_base(3, 2) + bif.log_dif_base(k, 3));
            if (y > b_max)
                y = b_max;
            else if (y < 0)
                y = 0;

            double min_delta = Math.Abs((y + (double)min_x * log_dif_base(3, 2) - bif.log_dif_base(k, 3)));
            for (int x = 0; x < a_max; x++)
            {
                y = (int)Math.Round(-x * log_dif_base(3, 2) + bif.log_dif_base(k, 3));
                if (y > b_max)
                    y = b_max;
                else if (y < 0)
                    y = 0;

                double delta = Math.Abs(y + x * log_dif_base(3, 2) - bif.log_dif_base(k, 3));
                if (min_delta > delta)
                {
                    min_x = x;
                    min_delta = delta;
                }
            }

            a = min_x;
            b = (int)Math.Round((double)-min_x * log_dif_base(3, 2) + bif.log_dif_base(k, 3));
            if (b > b_max)            
                b = b_max;

            List<int> r = new List<int>();
            r.Add(a);
            r.Add(b);
            return r;
        }

        public static List<int> Best_Approximation_2(BigInteger k, int a_max, int b_max)
        {
            int a = 0;
            int b = 0;
            int legth_k = get_number_bit(k, 2);
            int[,] PreComputation = new int[(int)b_max + 1, 3];
            int i;
            for (i = 0; i <= b_max; i++)
            {
                PreComputation[i, 0] = i;
                PreComputation[i, 1] = (int)Math.Pow(3, i);
                int temp = get_number_bit(PreComputation[i, 1], 2);
                if (temp > legth_k)
                {
                    b_max = i - 1;
                    break;
                }
                PreComputation[i, 2] = (PreComputation[i, 1] << (int)(legth_k - temp));
            }

            for (i = 0; i <= b_max; i++)
            {
                int i_min = i;
                int min = PreComputation[i, 2];
                for (int u = i + 1; u <= b_max; u++)
                {
                    if (min > PreComputation[u, 2])
                    {
                        i_min = u;
                        min = PreComputation[u, 2];
                    }
                }
                for (int j = 0; j < 3; j++)
                {
                    int temp = PreComputation[i_min, j];
                    PreComputation[i_min, j] = PreComputation[i, j];
                    PreComputation[i, j] = temp;
                }
            }
            i = (int)b_max + 1;
            int length_1 = 0;
            int length_max = 0;

            while (i > 0 && length_1 >= length_max)
            {
                int j = 0;
                length_max = length_1;
                string str1 = bif.ToBin(k);
                string str2 = Convert.ToString((PreComputation[i - 1, 2]), 2);
                while (j < (int)legth_k - 1 && j < Math.Min(str2.Length, str1.Length) && (str2[j] == str1[j]))
                {
                    j = j + 1;
                }
                if (j != 0)
                {
                    length_1 = (int)legth_k - ((int)legth_k - j);
                }
                else length_1 = (int)legth_k;
                i = i - 1;
            }
            if (length_1 < length_max)
            {
                i = i + 2;
            }
            else i = 1;

            int b1 = PreComputation[i - 1, 0];
            int a1 = legth_k - get_number_bit(PreComputation[i - 1, 1], 2);
            if (a1 < 0)
            {
                a1 = 0;
            }
            int a2, b2;
            if (i < b_max + 1)
            {
                b2 = PreComputation[i, 0];
                a2 = legth_k - get_number_bit(PreComputation[i, 1], 2);
                if (a2 < 0)
                {
                    a2 = 0;
                }
            }
            else
            {
                b2 = 0;
                a2 = 0;
            }

            Re_Compute_a_b(ref a1, ref b1, a_max, b_max, k);
            Re_Compute_a_b(ref a2, ref b2, a_max, b_max, k);
            if (Math.Abs((double)k - (double)Math.Pow(2, (double)a1) * (double)Math.Pow(3, (double)b1)) < Math.Abs((double)k - (double)Math.Pow(2, (double)a2) * (double)Math.Pow(3, (double)b2)))
            {
                a = a1;
                b = b1;
            }
            else
            {
                a = a2;
                b = b2;
            }
            if ((a != a_max) && (Math.Abs((double)k - (double)Math.Pow(2, a + 1) * (double)Math.Pow(3, b)) < Math.Abs((double)k - (double)Math.Pow(2, a) * (double)Math.Pow(3, b))))
            {
                a = a + 1;
            }
            List<int> r = new List<int>();
            r.Add(a);
            r.Add(b);
            return r;
        }

        public static double log_dif_base(int _base, int argument)
        {
            return (Math.Log(argument) / Math.Log(_base));
        }

        public static int get_number_bit(BigInteger value, int _base)
        {
            int number_bit = (int)Math.Floor(bif.log_dif_base(value, _base));
            if (((int)bif.log_dif_base(value, _base) > number_bit) || number_bit == 0)
                number_bit++;
            return number_bit;
        }

        public static void Re_Compute_a_b(ref int a, ref int b, int a_max, int b_max, BigInteger k)
        {
            if (a > a_max)
            {
                int temp = get_number_bit((int)Math.Pow(2, a - a_max), 3) - 1;
                b = b + temp;

                if (b > b_max)
                {
                    b = b_max;
                }
                if (a_max > 0)
                {
                    temp = a_max - 1;
                }
                else temp = 0;
                if ((Math.Abs((double)k - Math.Pow(2, a_max) * Math.Pow(3, b)) < Math.Abs((double)k - Math.Pow(2, temp) * Math.Pow(3, (b + 1)))) || b == b_max)
                {
                    a = a_max;
                }
                else
                {
                    a = temp;
                    b = b + 1;
                }
            }
        }
    }
}
