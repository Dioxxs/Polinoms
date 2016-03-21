using System;
using System.Collections.Generic;
using System.IO;
using System.Numerics;
using System.Linq; // for list.Max()

namespace Polinoms
{
    public class cpolinom
    {
        #region attributes
        private List<int> x = new List<int>();
        public int mod;
        private static Random r = new Random(DateTimeOffset.Now.Second + DateTimeOffset.Now.Minute * 60);
        #endregion
        #region constructors
        public static cpolinom CreateRand(int n, int m)
        {
            cpolinom c = new cpolinom("", m);
            for (int i = 0; i < n; i++)
                c.Add(Mod(r.Next(), m));
            while (c[0] == 0)
                c.RemoveAt(0);
            return c;
        }
        public cpolinom(int n, int m)
        {
            Add(1);
            for (int i = 0; i < n; i++)
                Add(0);
            mod = m;
        }
        public cpolinom(string s, int m)
        {
            mod = m;
            string t = "";
            for (int i = 0; i < s.Length; i++)
            {
                if (s[i] != ' ')
                    t = t + s[i];
                else
                {
                    Add(Mod(Convert.ToInt16(t), mod));
                    t = "";
                }
            }
            if (t != "")
                Add(Mod(Convert.ToInt16(t), mod));
        }
        #endregion
        #region InverseFuncs
        public static List<cpolinom> Primes(int n, int mod)
        {
            List<cpolinom> r = new List<cpolinom>();
            cpolinom t = new cpolinom("", mod);

            cpolinom temp = new cpolinom("1 0", mod);

            r.Add(Copy(temp));
            temp++;

            while (temp.Count != n + 2)
            {
                for (int i = 0; i < r.Count; i++)
                {
                    t = temp % r[i];
                    if (t.Count == 1 && t[0] == 0)
                        break;
                }
                if (t.Count > 1 || (t.Count == 1 && t[0] != 0))
                    r.Add(Copy(temp));
                temp++;
            }
            return r;
        }
        public cpolinom InvPolinom(cpolinom n)
        {
            cpolinom x = new cpolinom("1", n.mod);
            cpolinom y = Copy(this);
            cpolinom r;
            while (true)
            {
                r = (x * y) % n;
                if (r.x[0] == 1 && r.Count == 1)
                    break;
                else
                    x++;
            }
            return x;
        }
        public cpolinom EvkPolinom(cpolinom n)
        {
            cpolinom u = Copy(n);
            cpolinom v = Copy(this);

            cpolinom A = new cpolinom("1", n.mod);
            cpolinom B = new cpolinom("0", n.mod);
            cpolinom C = new cpolinom("0", n.mod);
            cpolinom D = new cpolinom("1", n.mod);

            cpolinom q;
            cpolinom t1;
            cpolinom t2;
            cpolinom t3;

            while (!v.isZero())
            {
                q = u / v;
                t1 = u - (q * v);
                while (t1.Count != 0)
                    if (t1[0] != 0)
                        break;
                    else t1.RemoveAt(0);
                t2 = A - (q * C);
                t3 = B - (q * D);

                u = Copy(v);
                A = Copy(C);
                B = Copy(D);

                v = Copy(t1);
                C = Copy(t2);
                D = Copy(t3);
            }
            if (B.Count == 1 && B[0] != 0)
                B = B / ((B * this) % n);//Якщо результат множення знайденого оберненого на заданний поліном дорівнює числу, то потрібно на це число розділити
            return B;
        }
        public cpolinom Inv2Polinom(cpolinom n)
        {
            cpolinom a = Copy(this);
            cpolinom x = new cpolinom("1 0", n.mod);
            cpolinom y1 = new cpolinom("1", n.mod);

            while (!a.isOne())
            {
                y1 = y1 * x % n;
                a = a * x % n;
            }
            return y1;
        }
        public cpolinom Inv3Polinom(cpolinom n)
        {
            cpolinom a = new cpolinom("1", n.mod);
            cpolinom b = Copy(this);
            cpolinom v = new cpolinom("0", n.mod);
            cpolinom p = Copy(n);
            int d = -1;

            p = new cpolinom("1 0 0 1 1", 2);
            a = new cpolinom("1 1 0 1", 2);
            b = new cpolinom("1 1 1", 2);

            StreamWriter sw = new StreamWriter("1234.txt");

            while (a.isZero() == false && p.isOne() == false)
            {
                sw.WriteLine(d + "|" + b.ToString() + "|" + p.ToString() + "|" + a.ToString() + "|" + v.ToString());
                if(b[b.Count - 1] == 1)
                {
                      if(d < 0)
                      {
                          b = b + p;
                          p = b - p;
                          a = a + v;
                          v = a - v;
                          d = -d;
                      }
                      else
                      {
                          b = b + p;
                          a = v + a;
                      }
                }
                b = b / (new cpolinom("1 0", n.mod));
                a = a / (new cpolinom("1 0", n.mod));
                d = d - 1;
            }
            sw.Close();
            return v;    

        }
        public cpolinom Evk2Polinom(cpolinom n)
        {
            cpolinom S = Copy(n);
            cpolinom R = Copy(this);

            cpolinom V = new cpolinom("0", n.mod);
            cpolinom U = new cpolinom("1", n.mod);

            while (R.Count != 1)
            {
                int d = S.Count - R.Count;
                if (d < 0)
                {
                    cpolinom t = Copy(S);
                    S = Copy(R);
                    R = Copy(t);

                    t = Copy(V);
                    V = Copy(U);
                    U = Copy(t);

                    d = -d;
                }
                cpolinom temp = new cpolinom(d, n.mod);
                S = S - temp * R;
                cpolinom.Fix(S);
                V = V - temp * U;
                cpolinom.Fix(V);                
            }
            return U/R;
        }
        public cpolinom VLSI(cpolinom n)
        {
            cpolinom S = Copy(n);
            cpolinom R = Copy(this);
            int m = S.Count - 1;
            int d = 0;

            cpolinom V = new cpolinom("0", n.mod);
            cpolinom U = new cpolinom("1", n.mod);
            cpolinom x = new cpolinom("1 0", n.mod);

            for(int i = 0; i < 2 * m; i++)
            {
                if (R.Count - 1 < m)
                {
                    R = R * x;
                    U = U * x;
                    d++;
                }
                else
                {
                    if (S.Count - 1 == m)
                    {
                        S = S - R;
                        V = V - U;
                        Fix(U);
                        Fix(S);
                    }
                    S = S * x;
                    if (d == 0)
                    {
                        cpolinom t = Copy(R);
                        R = Copy(S);
                        S = Copy(t);

                        t = Copy(U);
                        U = x * V;
                        V = Copy(t);

                        d++;
                    }
                    else
                    {
                        U = U / x;
                        d--;
                    }
                }                
            }
            return U;
        }
        #endregion
        #region BasicFuncs
        public static bool operator >(cpolinom a, cpolinom b)
        {
            if (a.Count > b.Count) return true;
            else if (a.Count < b.Count) return false;
            else
                for (int i = 0; i < a.Count; i++)
                    if (a[i] > b[i]) return true;
                    else if (a[i] < b[i]) return false;
            return false;
        }
        public static bool operator <(cpolinom a, cpolinom b)
        {
            if (a.Count < b.Count) return true;
            else if (a.Count > b.Count) return false;
            else
                for (int i = 0; i < a.Count; i++)
                    if (a[i] < b[i]) return true;
                    else if (a[i] > b[i]) return false;
            return false;
        }
        public static cpolinom operator +(cpolinom a, cpolinom b)
        {
            cpolinom np = new cpolinom("", Math.Max(a.mod, b.mod));
            while (a.Count < b.Count)
                a.Insert(0, 0);
            while (b.Count < a.Count)
                b.Insert(0, 0);
            for (int i = 0; i < a.Count; i++)
                np.Add(Mod(a[i] + b[i], np.mod));
            return np;
        }
        public static cpolinom operator -(cpolinom a, cpolinom b)
        {
            cpolinom np = new cpolinom("", Math.Max(a.mod, b.mod));
            while (a.Count < b.Count)
                a.Insert(0, 0);
            while (b.Count < a.Count)
                b.Insert(0, 0);
            for (int i = 0; i < a.Count; i++)
                np.Add(Mod(a[i] - b[i], np.mod));
            return np;
        }
        public static cpolinom operator *(cpolinom a, cpolinom b)
        {
            cpolinom np = new cpolinom("", Math.Max(a.mod, b.mod));
            cpolinom t;

            for (int i = 0; i < a.Count; i++)
                np.Add(a[i] * b[0]);

            for (int i = 1; i < b.Count; i++)
            {
                t = new cpolinom("", Math.Max(a.mod, b.mod));
                for (int j = 0; j < a.Count; j++)
                    t.Add(a[j] * b[i]);
                np.Add(0);
                np = np + t;
            }

            if (np.x.Count == 0)
                np.Insert(0, 0);
            while (np[0] == 0 && np.Count > 1)
                np.RemoveAt(0);

            return np;
        }
        public static cpolinom operator /(cpolinom a, cpolinom b)
        {
            cpolinom np = new cpolinom("", Math.Max(a.mod, b.mod));
            cpolinom t = Copy(a);

            int m;
            while (t.Count >= b.Count)
            {
                m = Coef(t[0], b[0], t.mod);
                if (m == 0) m = Mod(t[0] / b[0], t.mod);
                for (int i = 0; i < b.Count; i++)
                    t[i] = Mod(t[i] - m * b[i], t.mod);
                t.RemoveAt(0);
                np.Add(m);
            }

            return np;
        }
        public static cpolinom operator %(cpolinom a, cpolinom b)
        {
            cpolinom np = Copy(a);
            int m;
            while (np.Count >= b.Count)
            {
                m = Coef(np[0], b[0], np.mod);
                if (m == 0) m = Mod(np[0] / b[0], np.mod);
                for (int i = 0; i < b.Count; i++)
                    np[i] = Mod(np[i] - m * b[i], np.mod);
                np.RemoveAt(0);
            }

            if (np.Count == 0)
                np.Insert(0, 0);
            while (np[0] == 0 && np.Count > 1)
                np.RemoveAt(0);

            return np;
        }
        public static cpolinom operator ++(cpolinom a)
        {
            for (int j = a.Count - 1; j > -1; j--)
            {
                a[j] = Mod(a[j] + 1, a.mod);
                if (a[j] != 0) break;
            }
            if (a[0] == 0)
                a.Insert(0, 1);
            return a;
        }
        #endregion
        #region UsabilityFuncs
        public int this[int i]
        {
            get { return (i < Count) ? x[i] : 0; }
            set { x[i] = value; }
        }
        public int Count { get { return x.Count; } }
        public void Add(int item) { x.Add(item); }
        public void Insert(int pos, int item) { x.Insert(pos, item); }
        public void RemoveAt(int pos) { if (x.Count > 0) x.RemoveAt(pos); }

        public override string ToString()
        {
            string s = "";
            for (int i = 0; i < this.Count; i++)
                s = s + this[i].ToString() + " ";
            return s;
        }
        public void PrintPolinom(StreamWriter sw)
        {
            for (int i = 0; i < this.Count; i++)
                sw.Write(this[i] + " ");
        }
        public static cpolinom Copy(cpolinom a)
        {
            cpolinom t = new cpolinom("", a.mod);
            for (int i = 0; i < a.Count; i++)
                t.Add(a[i]);
            return t;
        }
        private static int Coef(int x1, int x2, int m)
        {
            for (int i = 0; i < m; i++)
                if (Mod(x2 * i, m) == x1)
                    return i;
            return 0;
        }
        private static int Mod(int x, int m)
        {
            int y = x;
            while (y < 0)
                y += m;
            return y%m;
        }
        private static void Fix(cpolinom p)
        {
            while (p[0] == 0 && p.Count != 0)
                p.RemoveAt(0);
            if (p.Count == 0) p.Add(0);
        }
        public bool isEqual(cpolinom p)
        {
            if (this.mod != p.mod)
                return false;
            else if (this.Count != p.Count)
                return false;
            else
            {
                for (int i = 0; i < this.Count; i++)
                    if (this[i] != p[i])
                        return false;
                return true;
            }
        }
        public bool isZero()
        {
            for (int i = 0; i < x.Count; i++)
                if (x[i] != 0)
                    return false;
            return true;
        }
        public bool isOne()
        {
            if (this.Count == 1 && this[0] == 1)
                return true;
            else return false;
        }
        #endregion
        #region Written by Alex Nycheporuk

        /// <summary>
        /// если старшие коеффициенты равны нулю, уменьшить порядок полинома
        /// </summary>
        private void CheckPolinomCount()
        {
            for (; ; )
            {
                if (x[Count - 1] == 0)
                    RemoveAt(Count - 1);
                else
                    break;
            }
        }
        private void CheckPolinomCount(int n)
        {
            for (; ; )
            {
                if (x[Count - n - 1] == 0)
                    RemoveAt(Count - 1);
                else
                    break;
            }
        }

        /// <summary>
        /// функция для нахождения числа, кратного 2^n для преобразования фурье
        /// например, 10 => 16, 4 => 4  7 => 8
        /// </summary>
        /// <param name="N"></param>
        /// <returns></returns>
        private int lengthFFT(int N)
        {
            int exp = 0;

            //N должен быть степенью 2, найдем найменьшее число степени 2, которое вмещает N
            if (--N <= 0) N = 1;
            else
            {
                for (int i = 1; ; i++)
                {
                    if (N == 1)
                    {
                        exp = i;
                        break;
                    }
                    N = N >> 1;
                }
                N = 1 << exp;
            }

            return N;
        }

        /// <summary>
        /// Умножение двух многочленов методом Фурье
        /// </summary>
        /// <param name="p"></param>
        /// <returns></returns>
        public cpolinom mul_FFT(cpolinom p, int method)
        {
            int N = lengthFFT(Count + p.Count);
            Complex[] a = new Complex[N];
            Complex[] b = new Complex[N];

            //заполним массивы Complex коеффициентами полиномов
            for (int i = 0; i < N; i++)
            {
                a[i] = this[i];
                b[i] = p[i];
            }

            //преобразуем коеффициенты по FFT
            a = FFT.fft(a, method);
            b = FFT.fft(b, method);

            //умножим полученные коеффициенты
            for (int i = 0; i < N; i++)
                a[i] *= b[i];

            //проведем обратное преобразование фурье
            a = FFT.ifft(a, method);

            //создадим полином с новыми коеффициентами
            cpolinom res = new cpolinom(N - 1, mod);
            for (int i = 0; i < N; i++)
            {
                //округлим значения до ближайшего
                try
                {
                    int k = (int)Math.Round(a[i].Real);
                    res[i] = k % mod;
                }
                catch (ArgumentOutOfRangeException e) { }
            }

            //пересчитаем степень полинома
            res.CheckPolinomCount(Zeros(this, p));

            return res;
        }
        private int Zeros(cpolinom a, cpolinom b)
        {
            int n = 0;
            for (int i = a.Count - 1; i > -1; i--)
            {
                if (a[i] == 0) n++;
                else break;
            }
            for (int i = b.Count - 1; i > -1; i--)
            {
                if (b[i] == 0) n++;
                else break;
            }
            return n;
        }
        #endregion
        #region Written by Viktoriya Romanchenko

        #endregion

        #region Written by Kate Gurenko
        
        public cpolinom meth2_49(cpolinom n)
        {
            cpolinom u = Copy(this);
            cpolinom v = Copy(n);
            cpolinom g1 = new cpolinom("1", n.mod);
            cpolinom g2 = new cpolinom("0", n.mod);

            while (u.Count != 1 && v.Count != 1)
            {
                Fix(u);
                Fix(v);
                Fix(g1);
                Fix(g2);
                cpolinom z = new cpolinom("1 0", n.mod);

                while ((u % z).isZero() == true)
                {
                    u = u / z;
                    if (g1.isZero() == true)
                        g1 = (g1 + n) / z;
                    while ((g1 % z).isZero() == false)
                        g1 = g1 + n;
                    g1 = g1 / z;
                }

                while ((v % z).isZero() == true)
                {
                    v = v / z;
                    if (g2.isZero() == true)
                        g2 = (g2 + n) / z;
                    while ((g2 % z).isZero() == false)
                        g2 = g2 + n;
                    g2 = g2 / z;                    
                }
                
                if (u.Count > v.Count)
                {
                    u = u + Copy(v);
                    g1 = g1 + g2;
                }
                else
                {
                    v = v + Copy(u);
                    g2 = g2 + g1;
                }
            }
            Fix(g1);
            Fix(g2);
            cpolinom g;
            if (u.Count == 1)
                g = g1;
            else
                g = g2;
            cpolinom t = ((g * this) % n);
            while (t.isOne() == false)
            {
                g = g + g;
                t = ((g * this) % n);
            }
            return g;
        }
        public cpolinom meth2_50(cpolinom n)
        {
            cpolinom u = Copy(this);
            cpolinom v = Copy(n);
            cpolinom g1 = new cpolinom("1", n.mod);
            cpolinom g2 = new cpolinom("0", n.mod);
            cpolinom z = new cpolinom("1 0", n.mod);
            int k = 0;

            while (u.isOne() == false && v.isOne() == false)
            {
                Fix(u);
                Fix(v);
                Fix(g1);
                Fix(g2);                

                while ((u % z).isZero() == true)
                {
                    u = u / z;
                    g2 *= z;
                    k++;
                }

                while ((v % z).isZero() == true)
                {
                    v = v / z;
                    g1 *= z;
                    k++;
                }

                if (u.Count > v.Count)
                {
                    u = u + Copy(v);
                    g1 += g2;
                }
                else
                {
                    v = v + Copy(u);
                    g2 += g1;
                }
            }
            cpolinom g;
            if (u.isOne() == true)
                g = g1;
            else
                g = g2;
            Fix(g);

            for (int i = 0; i < k; i++)
            {
                while ((g % z).isZero() == false)
                    g = g + n;
                g /= z;
            }
            return g;
        }

        #endregion
        #region written by Ivan Lubashenko
        public cpolinom(BigInteger bigint, int boundary, int m) // конструктор для розпакування поліному з BigInteger для методу bin_segmentation
        {
            //cpolinom res = new cpolinom("", m);
            mod = m;
            BigInteger mask = (1 << boundary) - 1;
            for (int i = 0; i < boundary; i++)
            {
                x.Add((int)(bigint & mask) % m);
                bigint >>= boundary;
            }
            x.Reverse();
            //return res;
        }
        public void square() // Algorithm 2.39 Polynomial squaring(p. 53)
        {					 // e. g.: (1, 1, 1) -> (1, 0, 1, 0, 1) i.e. (x^2 + x + 1) -> (x^4 + x^2 + 1) 
            for (int i = Count - 1; i > 0; i--)
            {
                x.Insert(i, 0);
            }
        }

        // Algorithm 9.6.1 (Fast polynomial multiplication: Binary segmentation)
        // також див. рядок 50 - конструктор public cpolinom(BigInteger bigint, int boundary, int m)

        // Функція для пакування коефіцієнтів полінома у bigint.
        // boundary - максимальна кількість біт, яку може займати коефіцієнт 
        // полінома-результату, тобто мінімальна кількість біт, яку необхідно виділити
        // для запису кожного коефіцієнта. Інакше(якщо виділити менше), при множенні bigint-представлення поліномів,
        // розряди двійкового представлення якогось коефіцієнта можуть "вилізти" за межі, виділені для його зберігання,
        // і змінити значення сусіднього коефіцієнта, а можливо і всіх наступних. За модулем коефіцієнти беруться вже
        // після множення і розпакування.
        public BigInteger pack(int boundary)
        {
            BigInteger res = new BigInteger();
            for (int i = 0; i < Count; i++)
            {
                res <<= boundary;	// зсув вліво на boundary біт
                res |= x[i];		// запис відповідного коефіцієнта
            }
            return res;
        }

        // Функція множення двох поліномів з використанням представлення їх як bigint
        // Напр.: cpolinom С = A.bin_seg(B), де А і В - деякі поліноми, а С - результат множення


        public static cpolinom meth9_6_1(cpolinom a, cpolinom b)
        {
            // Обчислення максимальної кількості біт, яку може займати коефіцієнт полінома-результату(!)
            // (це важливо, тому що для зберігання коефіцієнтів поліномів-множників вистачило б ]log2(p)[ біт,
            // де р - модуль за яким виконуються операції над коефіцієнтами полінома)
            // Формулу див. тут http://www.cs.berkeley.edu/~fateman/papers/polysbyGMP.pdf (стор. 3, абзац 2)
            int boundary = 2 * Math.Max(a.Count, b.Count) * a.x.Max() * b.x.Max();
            // пакування коефіцієнтів, множення, розпакування(див. рядок 50 - конструктор public cpolinom(BigInteger bigint, int boundary, int m))
            // відповідає крокам 2-4 алгоритму 9.6.1
            cpolinom res = new cpolinom(b.pack(boundary) * a.pack(boundary), boundary, a.mod);
            Fix(res);
            return res;
        }
        #endregion
        #region Written by Ksenia Romanova

        //Algorithm 2.33 Right-to-left shift-and-add field multiplication in F2m
        public static cpolinom meth2_33(cpolinom a, cpolinom b)
        {
            cpolinom c;         
            cpolinom t = Copy(b);

            if (a[a.Count - 1] != 0) c = Copy(t) * (new cpolinom("" + a[a.Count - 1] + "", b.mod));
            else c = new cpolinom("0", b.mod);

            for (int i = a.Count - 2; i > -1; i--)
            {
                t.Add(0);
                if (a[i] != 0) c = c + t * (new cpolinom("" + a[i] + "", b.mod));
            }
            return c;
        }

        public static cpolinom meth2_34(cpolinom a, cpolinom b)
        {
            // interface method that wraps computation contained
            // in lr_comb_mul_arrays() private method
            return new cpolinom(rl_comb_mul_arrays(a.ToArray(), b.ToArray()), 2);
        }

        private static uint[] rl_comb_mul_arrays(uint[] a, uint[] b)
        {
            //------------------------------------------------------------------------
            // Algorithm 2.35
            // INPUT : Binary polynomials a(z) and b(z) of degree at most m − 1.
            // OUTPUT : c(z) = a(z) · b(z).
            //    1. C <- 0.
            //    2. For k from W − 1 downto 0 do
            //       2.1 For j from 0 to t − 1 do
            //	     If the kth bit of A[ j ] is 1 then add B to C{j}.
            //       2.2 If k != 0 then C ← C · z.
            //    3. Return(C).
            //------------------------------------------------------------------------
            // operates on arrays, can be used from wrapper LRCombMul()
            uint[] c = new uint[Math.Max(a.Length, b.Length) + 1];
            for (int k = 0; k <= sizeof(uint) * 8; k++)
            {
                for (int j = 0; j < a.Length; j++)
                {
                    if ((a[j] & (1 << k)) != 0)
                    {
                        add_poly_arrays(c, b, j);
                    }
                }
                if (k != sizeof(uint) * 8) shift_array(b);
            }
            return c;
        }

        #endregion
        #region Written by Yuri Zhykin
        // Algorithms for left-to-right and right-to-left comb multiplication of
        // binary polinomials require representing them as arrays of words of size W
        // thus forming bit-matrices. Such a representation works only for binary
        // polynomials, but makes use of highly efficient bitwise operations to skip
        // redundant computations and improve performance the algorithm. It is not
        // suitable for polynomials of field p^m, as there is no reasonably efficient
        // way to perform bitwise operation on an arrays of numbers.
        //
        // This particular implementation uses arrays of uints, so each array element
        // (uint number) contains 32 coefficients of a polynomial.
        //
        // Conversions from current representation to bit-matrix representation:
        //   - constructor cpolinom(a, m)
        //        takes uint array and constructs a usual polynomial; uses
        //        uses array_2_bstring(a) utility function for internal conversion
        //   - ToArray()
        //        returns array of uint numbers, which can be passed to utility
        //        methods described below
        //
        // Public interface methods:
        //   - LRCombMul(p1, p2)
        //        wrapper around implementation of alg. 2.35; converts polynomials and
        //        passes them to lr_comb_mul_arrays(p1, p2) utility method; converts
        //        result back afterwards
        //   - WLRCombMul(p1, p2)
        //        wrapper around implementation of alg. 2.36; converts polynomials and
        //        passes them to wlr_comb_mul_arrays(p1, p2) utility method; converts
        //        result back afterwards
        //
        // Utility methods are defined:
        //   - array_2_bstring(a)
        //        converts uint array to a binary string, suitable to be
        //        passed to cpolinom(string s, int m) constructor
        //   - add_poly_array(p1, p2, shift)
        //        adds two polynomials starting from a given bit, stores result in p1;
        //        implements algorithm 2.32; is used in algorithms 2.35 and 2.36
        //   - lr_comb_mul_arrays(p1, p2)
        //        left-to-right comb method for polynomial multiplication;
        //        implementation of algorithm 2.35
        //   - wlr_comb_mul_arrays(p1, p2)
        //        left-to-right comb method with windows of width w;
        //        implementation of algorithm 2.36
        //
        // Other:
        //   - precompute_bu(a, w)
        //        implementation of Step 1 of algorithm 2.36
        //   - shift_array(a)
        //        shifts bit-matrix for one position to the left;
        //        represents operation of multiplying polynomial f(x) by x
        public cpolinom(uint[] arr, int m) : this(array_2_bstring(arr, m), m) { }

        public uint[] ToArray()
        {
            if (mod != 2)
                throw new NotSupportedException("Only binary polynomials supported.");
            int isize = sizeof(uint) * 8;
            string str = String.Join("", x);
            int size = (int)Math.Ceiling((double)str.Length / isize);
            str = str.PadLeft(size * isize, '0');
            uint[] arr = new uint[size];
            for (int i = 0; i < size; i++)
                arr[i] = Convert.ToUInt32(str.Substring(i * isize, isize), 2);
            Array.Reverse(arr);
            return arr;
        }

        public static cpolinom meth2_35(cpolinom a, cpolinom b)
        {
            // interface method that wraps computation contained
            // in lr_comb_mul_arrays() private method
            return new cpolinom(lr_comb_mul_arrays(a.ToArray(), b.ToArray()), 2);
        }

        public static cpolinom meth2_36(cpolinom a, cpolinom b)
        {
            // interface method that wraps computation contained
            // in wlr_comb_mul_arrays() private method
            return new cpolinom(wlr_comb_mul_arrays(a.ToArray(), b.ToArray()), 2);
        }

        public static string array_2_bstring(uint[] arr, int m)
        {
            if (m != 2)
                throw new NotSupportedException("Only binary polynomials supported.");
            string str = "";
            int isize = sizeof(uint) * 8;
            for (int i = arr.Length - 1; i >= 0; i--)
            {
                string bnum = Convert.ToString(arr[i], 2);
                str += String.Join(" ", bnum.PadLeft(isize, '0').ToCharArray()) + " ";
            }
            return str.TrimStart(new Char[] { '0', ' ' });
        }

        private static uint[] add_poly_arrays(uint[] a, uint[] b, int shift = 0)
        {
            //------------------------------------------------------------------------
            // Algorithm 2.32
            // INPUT : Binary polynomials a(z) and b(z) of degrees at most m − 1.
            // OUTPUT : c(z) = a(z) + b(z).
            //    1. For i from 0 to t − 1 do
            //       1.1 C[i ] ← A[i ] ⊕ B[i ].
            //    2. Return(c).
            //------------------------------------------------------------------------
            for (int i = 0; i < Math.Min(a.Length, b.Length); i++)
                a[i + shift] ^= b[i];
            return a;
        }

        private static uint[] lr_comb_mul_arrays(uint[] a, uint[] b)
        {
            //------------------------------------------------------------------------
            // Algorithm 2.35
            // INPUT : Binary polynomials a(z) and b(z) of degree at most m − 1.
            // OUTPUT : c(z) = a(z) · b(z).
            //    1. C <- 0.
            //    2. For k from W − 1 downto 0 do
            //       2.1 For j from 0 to t − 1 do
            //	     If the kth bit of A[ j ] is 1 then add B to C{j}.
            //       2.2 If k != 0 then C ← C · z.
            //    3. Return(C).
            //------------------------------------------------------------------------
            // operates on arrays, can be used from wrapper LRCombMul()
            uint[] c = new uint[Math.Max(a.Length, b.Length) + 1];
            for (int k = sizeof(uint) * 8; k >= 0; k--)
            {
                for (int j = 0; j < a.Length; j++)
                {
                    if ((a[j] & (1 << k)) != 0)
                    {
                        add_poly_arrays(c, b, j);
                    }
                }
                if (k != 0) shift_array(c);
            }
            return c;
        }

        public static uint[] wlr_comb_mul_arrays(uint[] a, uint[] b)
        {
            //------------------------------------------------------------------------
            // Algorithm 2.36
            // INPUT : Binary polynomials a(z) and b(z) of degree at most m - 1.
            // OUTPUT : c(z) = a(z) · b(z).
            //    1. Compute bu=u(z)*b(z) for all polynomials u(z) of degree at most w-1.
            //    2. C <- 0.
            //    3. For k from (W/w) − 1 downto 0 do
            //       3.1 For j from 0 to t − 1 do
            //          Let u = (u(w-1),..., u1, u0), where ui is bit (wk + i) of A[j]
            //          Add bu to C{j}.
            //       3.2 If k != 0 then C <- C · z^w .
            //    4. Return(C)
            //------------------------------------------------------------------------
            int w = 4; // choose w such that W (in our case 32) is integral
            uint[][] BU = precompute_bu(b, w); // compute u(z) * b(z) for all u(z) of deg. w - 1
            uint[] c = new uint[Math.Max(a.Length, b.Length) + 2];
            for (int k = sizeof(uint) * 8 / w; k >= 0; k--)
            {
                for (int j = 0; j < a.Length; j++)
                {
                    int u = 0;
                    for (int i = 0; i < w; i++)
                        if ((a[j] & (1 << (w * k + i))) != 0)
                            u |= 1 << i;
                    add_poly_arrays(c, BU[u], j);
                }
                if (k != 0)
                    for (int i = 0; i < w; i++)
                        shift_array(c);
            }
            return c;
        }

        private static void shift_array(uint[] arr)
        {
            // shift array for one bit to the left, propagating the carry
            bool flag = false;
            for (int i = 0; i < arr.Length; i++)
            {
                uint newval = arr[i] << 1;
                if (flag) newval |= 1;
                if (arr[i] == uint.MaxValue) flag = true;
                else flag = false;
                arr[i] = newval;
            }
        }

        private static uint[][] precompute_bu(uint[] b, int w)
        {
            int usize = 1 << w;
            uint[][] bu = new uint[usize][];
            for (int i = 0; i < usize; i++)
                bu[i] = lr_comb_mul_arrays(new uint[] { (uint)i }, b);
            return bu;
        }
        #endregion

        # region Gordienko

        private static cpolinom CountMnojPolinim(cpolinom a, int tmp)
        {
            cpolinom t = cpolinom.Copy(a);

            for (int i = 0; i < t.Count; i++)
                t[i] *= tmp % t.mod;

            return t;
        }

        private static cpolinom CountDilPolinim(cpolinom a, int tmp)
        {
            for (int i = 0; i < a.x.Count; i++)
                if (tmp != 0 && a.x[i] != 0)
                    a.x[i] /= tmp;

            return a;
        }

        public static cpolinom Bit_Level(cpolinom a, cpolinom b, cpolinom n, int k)
        {
            /*
                Bit-Level Algorithm for Montgomery Multiplication
                Input: a(x), b(x), n(x)
                Output: c(x) = a(x)b(x)x−k mod n(x)
                Step 1. c(x) := 0
                Step 2. for i = 0 to k − 1 do
                Step 3. c(x) := c(x) + aib(x)
                Step 4. c(x) := c(x) + c0n(x)
                Step 5. c(x) := c(x)/x
             */

            cpolinom c = new cpolinom("0", a.mod);
            cpolinom x = new cpolinom("1 0", a.mod);
            cpolinom t = new cpolinom("1", a.mod);
            int f = 0;

            // Step 2,3,4,5
            for (int i = a.Count - 1; i > -1; i--)
            {
                if (f < k)
                {
                    c = c + CountMnojPolinim(b, a[i]);
                    while (c[c.Count - 1] != 0)
                        c = c + n;

                    Fix(n);
                    c = c / x;
                    f++;
                }
                else
                {
                    c = (c + (CountMnojPolinim(b, a[i]) * t)) % n;
                    t = t * x;
                }
            }

            return c;
        }

        public static cpolinom Bit_Level_Squaring(cpolinom a, cpolinom n, int k)
        {
            /*
                Input: a(x), n(x)
                Output: c(x) = a2(x)x−k mod n(x)
                Step 1. c(x) := k−1 i=0 aix2i
                Step 2. for i = 0 to k − 1 do
                Step 3. c(x) := c(x) + c0n(x)
                Step 4. c(x) := c(x)/x
             */

            cpolinom c = new cpolinom("", a.mod);
            cpolinom x = new cpolinom("1 0", a.mod);


            for (int i = 0; i < a.Count ; i++)
            {
                c.Add(a[i]);
                c.Add(0);
            }
            c.RemoveAt(c.Count - 1);

            // Step 1,2,3,4
            for (int i = 0; i < k; i++)
            {
                while (c[c.Count - 1] != 0)
                    c = c + n;

                Fix(n);
                c = c / x;
            }

            return c;
        }

        public cpolinom Inversion_Algorithm(cpolinom n)
        {
            /*
                Step 1. N0(x) := 1
                Step 2. for i = 2 to w
                Step 3. t(x) := N0(x)N0(x) mod xi
                Step 4. if t(x) = 1 then N0(x) := N0(x) + xi−1
            */

            cpolinom N0 = new cpolinom("1", this.mod);
            cpolinom t = new cpolinom("0", this.mod);

            // N0, w
            while(t.isOne() == false)
            {
                t = this * N0 % n;
                N0 = N0 + n;
            }

            return N0;
        }

        public static cpolinom Word_Level(cpolinom a, cpolinom b, cpolinom n, int N_zero)
        {
            /*
                Step 1. c(x) := 0
                Step 2. for i = 0 to s − 1 do
                Step 3. c(x) := c(x) + Ai(x)b(x)
                Step 4. M(x) := C0(x)N0(x) (mod xw)
                Step 5. c(x) := c(x) + M(x)n(x)
                Step 6. c(x) := c(x)/xw
            */
            // Step 1
            cpolinom c = new cpolinom("", a.x.Count);
            int m = 0;
            // Step 2,3,4,5,6
            for (int i = 0; i < a.Count - 1; i++)
            {
                c = c + CountMnojPolinim(b, a[i]);
                m = Convert.ToInt32((c[0] * N_zero) % Math.Pow(c[0], a.Count - 1));
                c = c + CountMnojPolinim(n, m);
                c = CountDilPolinim(c, Convert.ToInt32(Math.Pow(c[0], a.Count - 1)));
            }

            return c;
        }

        public static cpolinom Montgomery_Squaring(cpolinom a, cpolinom n, int N_zero)
        {
            /*
                Step 1. c(x) := k−1i=0 aix2i
                Step 2. for i = 0 to s − 1 do
                Step 3. M(x) := C0(x)N0(x) (mod xw)
                Step 4. c(x) := c(x) + M(x)n(x)
                Step 5. c(x) := c(x)/xw
             */
            cpolinom c = new cpolinom("", a.mod);

            // Step 1
            for (int i = 0; i < a.x.Count; i++)
            {
                int Element = 0;
                for (int j = 0; j < a.Count; j++)
                    Element += Convert.ToInt32(a[i] * Math.Pow(a[i], 2 * i));

                c.Add(Element);
            }
            // Step 2,3,4,5
            for (int i = 0; i < Convert.ToInt32(a.Count / (a.Count - 1)); i++)
            {
                int m = c.x[i] * N_zero;
                if (Math.Pow(c[0], a.Count - 1) != 0 && c[i] * N_zero != 0)
                    m = Convert.ToInt32(c[i] * N_zero % Math.Pow(c[0], a.Count - 1));

                c = c + CountMnojPolinim(n, m);
                c = CountDilPolinim(c, Convert.ToInt32(a.Count - 1));

                if (i < Convert.ToInt32(a.Count / (a.Count - 1)))
                    break;
            }

            return c;
        }

        #endregion
    }
}
