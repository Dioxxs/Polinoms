using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using System.Numerics;


namespace Polinoms
{
    public partial class Form1 : Form
    {
        private static List<MPower> PowFuncs = new List<MPower>();
        private static List<MInverse> InvFuncs = new List<MInverse>();
        private static List<MMulti> MultiFuncs = new List<MMulti>();
        private static List<MOperation> ops = new List<MOperation>();

        private static flag act = new flag();
        private string CorrectName(string s)
        {
            string t = s.Remove(s.Length - 4);
            string r = "";
            for (int i = t.Length - 1; t[i] != '\\'; i--)
                r = t[i] + r;
            return r;
        }
        private int FindP(string st)
        {
            string s = st;
            string t = "";
            for (int i = 0; s[i] != ','; i++)
                t = t + s[i];
            return Convert.ToInt32(t);
        }
        private int FindN(string st)
        {
            string s = st;
            string t = "";
            int i = 0;
            for (i = 0; s[i] != ','; i++) ;
            for (i++; i < s.Length; i++)
                t = t + s[i];
            return Convert.ToInt32(t);
        }

        private List<int> ReadString(string s)
        {
            string st = "";
            List<string> sts = new List<string>();
            for (int i = 0; i < s.Length; i++)
            {
                if (s[i] == ' ')
                    continue;
                if (s[i] != ';')
                    st = st + s[i];
                else
                {
                    sts.Add(st);
                    st = "";
                }
            }
            if (st != "")
            {
                sts.Add(st);
                st = "";
            }
            
            List<int> x = new List<int>();
            List<int> pows = new List<int>();
            for (int i = 0; i < sts.Count; i++)
            {
                x.Clear();
                for (int j = 0; j < sts[i].Length; j++)
                {
                    if (sts[i][j] != '-')
                        st = st + sts[i][j];
                    if (sts[i][j] == '-')
                    {
                        x.Add(Convert.ToInt16(st));
                        st = "";
                    }
                }
                if (st != "")
                {
                    x.Add(Convert.ToInt16(st));
                    st = "";
                }

                if (x.Count == 1)
                    pows.Add(x[0]);
                else 
                {
                    int step;
                    if (x.Count == 2)
                        step = 1;
                    else
                        step = x[2];
                    for (int j = x[0]; j <= x[1]; j += step)
                        pows.Add(j);
                }          
            }
            pows.Sort();
            for (int i = 0; i < pows.Count - 1; i++)
            {
                if (pows[i] == pows[i + 1])
                {
                    pows.RemoveAt(i);
                    i--;
                }
            }
            return pows;
        }
        private List<BigInteger> ReadString2(string s)
        {
            string st = "";
            List<string> sts = new List<string>();
            for (int i = 0; i < s.Length; i++)
            {
                if (s[i] == ' ')
                    continue;
                if (s[i] != ';')
                    st = st + s[i];
                else
                {
                    sts.Add(st);
                    st = "";
                }
            }
            if (st != "")
            {
                sts.Add(st);
                st = "";
            }

            List<BigInteger> x = new List<BigInteger>();
            List<BigInteger> pows = new List<BigInteger>();
            for (int i = 0; i < sts.Count; i++)
            {
                x.Clear();
                for (int j = 0; j < sts[i].Length; j++)
                {
                    if (sts[i][j] != '-')
                        st = st + sts[i][j];
                    if (sts[i][j] == '-')
                    {
                        x.Add(BigInteger.Parse(st));
                        st = "";
                    }
                }
                if (st != "")
                {
                    x.Add(BigInteger.Parse(st));
                    st = "";
                }

                if (x.Count == 1)
                    pows.Add(x[0]);
                else
                {
                    BigInteger step;
                    if (x.Count == 2)
                        step = 1;
                    else
                        step = x[2];
                    for (BigInteger j = x[0]; j <= x[1]; j += step)
                        pows.Add(j);
                }
            }
            pows.Sort();
            for (int i = 0; i < pows.Count - 1; i++)
            {
                if (pows[i] == pows[i + 1])
                {
                    pows.RemoveAt(i);
                    i--;
                }
            }
            return pows;
        }
        public Form1()
        {
            InitializeComponent();
            Parse();
            FillFuncs();
            checkedListBox1.SetItemChecked(0, true);
            checkedListBox1.SetItemChecked(1, true);
            checkedListBox2.SetItemChecked(1, true);
            checkedListBox3.SetItemChecked(0, true); 
            StreamReader sr = new StreamReader("Temp\\Pow.txt");
            string date = sr.ReadLine();
            sr.Close();
            button11.Text = button11.Text + " " + date;
        }

        private void Parse()
        {
            listBox2.Items.Clear();
            Parser pars = new Parser();
            StreamReader sr = new StreamReader("Special Polinoms.txt");
            cpolinom pol;
            while (sr.EndOfStream == false)
            {
                string str = sr.ReadLine();
                pol = pars.ParseString(str);
                listBox2.Items.Add((pol.mod).ToString() + "," + (pol.Count - 1).ToString());
            }
            sr.Close();
        }

        private void FillFuncs()
        {
            ops.Add(new MOperation((a, b) => a + b, "Addition"));
            ops.Add(new MOperation((a, b) => a - b, "Substraction"));
            ops.Add(new MOperation((a, b) => a / b, "Division"));
            ops.Add(new MOperation((a, b) => a % b, "Mod"));

            InvFuncs.Add(new MInverse((p, ir) => p.InvPolinom(ir), "Inv1"));
            InvFuncs.Add(new MInverse((p, ir) => p.EvkPolinom(ir), "Euclid"));
            InvFuncs.Add(new MInverse((p, ir) => p.Inv2Polinom(ir), "Inv2"));
            InvFuncs.Add(new MInverse((p, ir) => p.Evk2Polinom(ir), "Euclid2"));
            InvFuncs.Add(new MInverse((p, ir) => p.VLSI(ir), "VSLI"));
            InvFuncs.Add(new MInverse((p, ir) => p.meth2_49(ir), "meth 2.49"));
            InvFuncs.Add(new MInverse((p, ir) => p.meth2_50(ir), "meth 2.50"));

            MultiFuncs.Add(new MMulti((a, b) => a * b, "School"));
            MultiFuncs.Add(new MMulti((a, b) => a.mul_FFT(b, 0), "FFT_1"));
            MultiFuncs.Add(new MMulti((a, b) => a.mul_FFT(b, 1), "FFT_2"));
            MultiFuncs.Add(new MMulti((a, b) => a.mul_FFT(b, 2), "FFT_3"));
            MultiFuncs.Add(new MMulti((a, b) => cpolinom.meth2_33(a, b), "Meth2_33"));
            MultiFuncs.Add(new MMulti((a, b) => cpolinom.meth2_34(a, b), "Meth2_34"));
            MultiFuncs.Add(new MMulti((a, b) => cpolinom.meth2_35(a, b), "Meth2_35"));
            MultiFuncs.Add(new MMulti((a, b) => cpolinom.meth2_36(a, b), "Meth2_36"));
            MultiFuncs.Add(new MMulti((a, b) => cpolinom.meth9_6_1(a, b), "Meth9_6_1"));

            for (int i = 0; i < MultiFuncs.Count; i++)
            {
                int k = i;
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.BinaryRL(pols[1], pols[0], MultiFuncs[k].mul, n), "1.BinaryRL, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.BinaryLR(pols[1], pols[0], MultiFuncs[k].mul, n), "2.BinaryLR, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.WindowRL(pols[1], pols[0], MultiFuncs[k].mul, n, w), "3.WindowRL, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.WindowLR(pols[1], pols[0], MultiFuncs[k].mul, n, w), "4.WindowLR, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.SlideRL(pols[1], pols[0], MultiFuncs[k].mul, n, w), "5.SlideRL, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.SlideRL(pols[1], pols[0], MultiFuncs[k].mul, n, w), "6.SlideLR, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.NAFBinaryRL(pols[1], pols[0], MultiFuncs[k].mul, n, inv), "7.NAFBinaryRL, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.meth7_2(pols[1], pols[0], MultiFuncs[k].mul, n, inv), "7_2.NAFBinaryRL, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.NAFBinaryLR(pols[1], pols[0], MultiFuncs[k].mul, n, inv), "8.NAFBinaryLR, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.NAFSlideRL(pols[1], pols[0], MultiFuncs[k].mul, n, w, inv), "9.NAFSlideRL, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.NAFSlideLR(pols[1], pols[0], MultiFuncs[k].mul, n, w, inv), "10.NAFSlideLR, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.NAFWindowRL(pols[1], pols[0], MultiFuncs[k].mul, n, w, inv), "11.NAFWindowRL, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.NAFWindowLR(pols[1], pols[0], MultiFuncs[k].mul, n, w, inv), "12.NAFWindowLR, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.wNAFSlideRL(pols[1], pols[0], MultiFuncs[k].mul, n, w, inv), "13.wNAFSlideRL, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.wNAFSlideLR(pols[1], pols[0], MultiFuncs[k].mul, n, w, inv), "14.wNAFSlideLR, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.AddSubRL(pols[1], pols[0], MultiFuncs[k].mul, n, inv), "15.AddSubRL, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.AddSubLR(pols[1], pols[0], MultiFuncs[k].mul, n, inv), "16.AddSubLR, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.Joye_double_and_add(pols[1], pols[0], MultiFuncs[k].mul, n), "17.JoyeDoubleAndAdd, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.MontgomeryLadder(pols[1], pols[0], MultiFuncs[k].mul, n), "18.MontgomeryLadder, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.DBNS1RL(pols[1], pols[0], MultiFuncs[k].mul, n, A, B, inv), "19.DBNS1RL, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.DBNS1LR(pols[1], pols[0], MultiFuncs[k].mul, n, A, B, inv), "20.DBNS1LR, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.DBNS2RL(pols[1], pols[0], MultiFuncs[k].mul, n, inv), "21.DBNS2RL, " + MultiFuncs[k].name));
                PowFuncs.Add(new MPower((pols, n, w, A, B, inv) => Pow.DBNS2LR(pols[1], pols[0], MultiFuncs[k].mul, n, inv), "22.DBNS2LR, " + MultiFuncs[k].name));
            }
        }

        private bool isWindow(int j)
        {
            if ((j > 1 && j < 6) || (j > 8 && j < 15))
                return true;
            else if ((j > 24 && j < 29) || (j > 31 && j < 41))
                return true;
            else
                return false;
        }
        private bool isNaf(int j)
        {
            if ((j > 5 && j < 17) || (j > 18 && j < 23))
                return true;
            if ((j > 28 && j < 40) || (j > 41 && j < 46))
                return true;
            else
                return false;
        }

        private void button6_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.OpenFileDialog openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
            if (openFileDialog1.ShowDialog() == System.Windows.Forms.DialogResult.OK)
            {
                string s = openFileDialog1.FileName;
                int p = FindP(CorrectName(s));
                StreamReader sr = new StreamReader(s);
                List<cpolinom> pols = new List<cpolinom>();
                cpolinom pol;
                cpolinom t;

                listBox3.Items.Clear();

                string str = sr.ReadLine();
                str = str.Substring(1, str.Length - 2);
                pols.Add(new cpolinom(str, p));
                sr.ReadLine();
                pols.Add(new cpolinom(sr.ReadLine(), p));
                pols.Add(new cpolinom(sr.ReadLine(), p));

                for (; ;)
                {
                    t = pols[2] * pols[1] % pols[0];
                    pol = cpolinom.Bit_Level(pols[2], pols[1], pols[0]);
                    
                    if(pol.isEqual(t) == false)
                        listBox3.Items.Add("(" + pols[2] + ").Inv(" + pols[1] + ")");

                    pols.RemoveAt(1);
                    if (sr.EndOfStream == true) break;
                        pols.Add(new cpolinom(sr.ReadLine(), p));
                }
                sr.Close();
                System.Windows.Forms.MessageBox.Show("Done");
            }
        }

        private void button1_Click(object sender, EventArgs e)
        {
            try
            {
                int p = Convert.ToInt32(textBox1.Text);
                int n = Convert.ToInt32(textBox2.Text);
                List<cpolinom> x = cpolinom.Primes(n, p);
                int size = x[x.Count - 1].Count;
                while (x[0].Count != size)
                    x.RemoveAt(0);
                StreamWriter w = new StreamWriter(p.ToString() + "," + n.ToString() + ".txt");
                for (int i = 0; i < x.Count; i++)
                {
                    x[i].PrintPolinom(w);
                    w.WriteLine("");
                }
                w.Close();
                System.Windows.Forms.MessageBox.Show("Done");
            }
            catch (FormatException exc) { System.Windows.Forms.MessageBox.Show("Error"); }
        }

        private void button5_Click(object sender, EventArgs e)
        {
            try
            {
                if (listBox2.SelectedIndex < 0) throw new FormatException();
                string s = listBox2.Items[listBox2.SelectedIndex].ToString();
                int p = FindP(s);
                int n = FindN(s);

                Parser pars = new Parser();
                StreamReader sr = new StreamReader("Special Polinoms.txt");
                cpolinom pol = new cpolinom("", p);

                while (sr.EndOfStream == false)
                {
                    string str = sr.ReadLine();
                    pol = pars.ParseString(str);
                    if (pol.mod == p && pol.Count - 1 == n)
                        break;                    
                }
                sr.Close();

                StreamWriter w = new StreamWriter(p.ToString() + "," + n.ToString() + ".txt");
                pol.PrintPolinom(w);
                w.Close();

                System.Windows.Forms.MessageBox.Show("Done");
            }
            catch (FormatException exc) { System.Windows.Forms.MessageBox.Show("Error"); }
            catch (IndexOutOfRangeException exc) { System.Windows.Forms.MessageBox.Show("Error"); }
        }
            
        private void button2_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.OpenFileDialog openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
            if (openFileDialog1.ShowDialog() == System.Windows.Forms.DialogResult.OK)
            {
                StreamReader sr = new StreamReader(openFileDialog1.FileName);

                listBox1.Items.Clear();

                while (sr.EndOfStream == false)
                    listBox1.Items.Add(sr.ReadLine());

                sr.Close();

                StreamWriter w = new StreamWriter("temp.txt");
                w.Write(CorrectName(openFileDialog1.FileName));
                w.Close();
            }
            System.Windows.Forms.MessageBox.Show("Done");
        }

        private void button3_Click(object sender, EventArgs e)
        {
            try
            {
                StreamReader r = new StreamReader("temp.txt");
                string s = r.ReadLine();
                r.Close();
                int p = FindP(s);
                int n = FindN(s);
                int count = Convert.ToInt32(textBox3.Text);

                if (listBox1.SelectedIndex < 0 || count < 0) throw new FormatException();

                StreamWriter w = new StreamWriter(p.ToString() + "," + n.ToString() + "," + count.ToString() + ".txt");

                w.WriteLine("[" + listBox1.Items[listBox1.SelectedIndex].ToString() + "]");

                cpolinom c = new cpolinom(listBox1.Items[listBox1.SelectedIndex].ToString(), p);
                cpolinom d;

                for (int i = 0; i < count; i++)
                {
                    d = cpolinom.CreateRand(n, p);
                    if (c > d)
                    {
                        w.WriteLine("");
                        d.PrintPolinom(w);
                    }
                    else i--;
                }
                w.Close();
                System.Windows.Forms.MessageBox.Show("Done");
            }
            catch (FormatException exc) { System.Windows.Forms.MessageBox.Show("Choose polinom"); }
            catch (IndexOutOfRangeException exc) { System.Windows.Forms.MessageBox.Show("Error"); }
        }

        private void button4_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.OpenFileDialog openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
            if (openFileDialog1.ShowDialog() == System.Windows.Forms.DialogResult.OK)
            {
                string s = openFileDialog1.FileName;                                
                int p = FindP(CorrectName(s));

                Directory.CreateDirectory("Operation results");
                StreamWriter sw = new StreamWriter("Operation results/result.csv");

                for (int i = 0; i < checkedListBox5.CheckedIndices.Count; i++)
                {
                    sw.Write(MultiFuncs[checkedListBox5.CheckedIndices[i]].name + ";");
                    sw.Write(oper(MultiFuncs[checkedListBox5.CheckedIndices[i]], s, p)+"\n");
                }
                for (int i = 0; i < checkedListBox4.CheckedIndices.Count; i++)
                {
                    sw.Write(InvFuncs[checkedListBox4.CheckedIndices[i]].name + ";");
                    sw.Write(oper(InvFuncs[checkedListBox4.CheckedIndices[i]], s, p)+"\n");
                }
                for (int i = 0; i < checkedListBox6.CheckedIndices.Count; i++)
                {
                    sw.Write(ops[checkedListBox6.CheckedIndices[i]].name + ";");
                    sw.Write(oper(ops[checkedListBox6.CheckedIndices[i]], s, p) + "\n");
                }
                sw.Close();
            }
        }

        private double oper(MMulti m, string fname, int p)
        {
            Stopwatch stw = new Stopwatch();
            StreamReader sr = new StreamReader(fname);
            StreamWriter sw = new StreamWriter("Operation results\\" + m.name + ".txt");
            List<cpolinom> pols = new List<cpolinom>();

            string str = sr.ReadLine();
            str = str.Substring(1, str.Length - 2);
            pols.Add(new cpolinom(str, p));
            sr.ReadLine();

            float sum = 0;
            int k = 0;
            for (; sr.EndOfStream == false; k++)
            {
                pols.Add(new cpolinom(sr.ReadLine(), p));

                stw.Start();
                m.mul(pols[1], pols[0]);
                stw.Stop();

                sw.WriteLine(stw.Elapsed.TotalMilliseconds);
                sum += (float)stw.Elapsed.TotalMilliseconds;

                pols.RemoveAt(0);                
                stw.Reset();
            }

            sw.WriteLine("");
            sw.WriteLine("Whole time:" + sum);
            sw.Write("Averange time:" + (double)sum / k);

            sw.Close();
            sr.Close();

            listBox5.Items.Add(m.name + " done ");
            listBox5.Update();

            return (double)sum / k;
        }
        private double oper(MInverse inv, string fname, int p)
        {
            Stopwatch stw = new Stopwatch();
            StreamReader sr = new StreamReader(fname);
            StreamWriter sw = new StreamWriter("Operation results\\" + inv.name + ".txt");
            List<cpolinom> pols = new List<cpolinom>();

            string str = sr.ReadLine();
            str = str.Substring(1, str.Length - 2);
            pols.Add(new cpolinom(str, p));
            sr.ReadLine();

            float sum = 0;
            int k = 0;
            for (; sr.EndOfStream == false; k++)
            {
                pols.Add(new cpolinom(sr.ReadLine(), p));

                stw.Start();
                inv.inv(pols[1], pols[0]);
                stw.Stop();

                sw.WriteLine(stw.Elapsed.TotalMilliseconds);
                sum += (float)stw.Elapsed.TotalMilliseconds;

                pols.RemoveAt(1);                
                stw.Reset();
            }

            sw.WriteLine("");
            sw.WriteLine("Whole time:" + sum);
            sw.Write("Averange time:" + (double)sum / k);

            sw.Close();
            sr.Close();

            listBox5.Items.Add(inv.name + " done ");
            listBox5.Update();

            return (double)sum / k;
        }
        private double oper(MOperation op, string fname, int p)
        {
            Stopwatch stw = new Stopwatch();
            StreamReader sr = new StreamReader(fname);
            StreamWriter sw = new StreamWriter("Operation results\\" + op.name + ".txt");
            List<cpolinom> pols = new List<cpolinom>();

            string str = sr.ReadLine();
            str = str.Substring(1, str.Length - 2);
            pols.Add(new cpolinom(str, p));
            sr.ReadLine();

            float sum = 0;
            int k = 0;
            for (; sr.EndOfStream == false; k++)
            {
                pols.Add(new cpolinom(sr.ReadLine(), p));

                stw.Start();
                op.op(pols[1], pols[0]);
                stw.Stop();

                sw.WriteLine(stw.Elapsed.TotalMilliseconds);
                sum += (float)stw.Elapsed.TotalMilliseconds;

                pols.RemoveAt(0);                
                stw.Reset();
            }

            sw.WriteLine("");
            sw.WriteLine("Whole time:" + sum);
            sw.Write("Averange time:" + (double)sum / k);

            sw.Close();
            sr.Close();

            listBox5.Items.Add(op.name + " done ");
            listBox5.Update();

            return (double)sum / k;
        }

        private void button7_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.OpenFileDialog openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
            if (openFileDialog1.ShowDialog() == System.Windows.Forms.DialogResult.OK)
            {
                act.Value = true;                
                string s = openFileDialog1.FileName;
                int p = FindP(CorrectName(s));
                List<BigInteger> N = ReadString2(textBox7.Text);
                List<int> W = ReadString(textBox10.Text);
                List<int> A = ReadString(textBox11.Text);
                List<int> B = ReadString(textBox12.Text);
                List<cpolinom> pols = new List<cpolinom>();

                StreamReader sr = new StreamReader(s);
                string str = sr.ReadLine();
                str = str.Substring(1, str.Length - 2);
                pols.Add(new cpolinom(str, p));
                sr.ReadLine();

                while (sr.EndOfStream == false)                
                    pols.Add(new cpolinom(sr.ReadLine(), p));
                sr.Close();

                DateTime d = DateTime.Now;
                string date = d.Year + "-";
                if (d.Month < 10) date += "0";
                date += d.Month + "-";
                if (d.Day < 10) date += "0";
                date += d.Day + " ";
                if (d.Hour < 10) date += "0";
                date += d.Hour + "-";
                if (d.Minute < 10) date += "0";
                date += d.Minute;

                Directory.CreateDirectory(date);

                List<int> ci1 = new List<int>();
                for (int f = 0; f < checkedListBox3.CheckedIndices.Count; f++)
                {
                    if (p > 2 && (checkedListBox3.CheckedIndices[f] > 4 && checkedListBox3.CheckedIndices[f] < 8))
                        continue;
                    for (int i = 0; i < checkedListBox1.CheckedIndices.Count; i++)
                        ci1.Add(checkedListBox1.CheckedIndices[i] + checkedListBox3.CheckedIndices[f] * checkedListBox1.Items.Count);
                }

                List<int> ci2 = new List<int>();
                for (int i = 0; i < checkedListBox2.CheckedIndices.Count; i++)
                    ci2.Add(checkedListBox2.CheckedIndices[i]);

                args a = new args(pols, ci1, ci2, W, A, B, N, 0, 0, 0, 0, 0, 0, date);                

                listBox4.Items.Clear();

                Thread th = new Thread(Pows);
                th.Start(a);    
            }
        }

        private void Pows(object obj)
        {
            args a = (args)obj;
            List<cpolinom> pols = a.pols;
            List<int> pinds = a.PowInds;
            List<int> iinds = a.InvInds;
            List<int> W = a.W;
            List<int> A = a.A;
            List<int> B = a.B;
            List<BigInteger> N = a.N;
            int pstart = a.pstart;
            int istart = a.istart;
            int wstart = a.wstart;
            int astart = a.astart;
            int bstart = a.bstart;
            int nstart = a.nstart;
            string date = a.date;

            string s = "";
            WriteInfo(N, W, A, B, date + "\\info.txt");
            for (int i = pstart; i < pinds.Count; i++)
            {
                int ii = pinds[i];

                if (isNaf(ii) == true)
                {
                    for (int j = istart; j < iinds.Count; j++)
                    {
                        int jj = iinds[j];
                        s = func1(ii, wstart, astart, bstart, nstart, pols, N, W, A, B, act, PowFuncs[ii], InvFuncs[jj]);
                        if (File.Exists(date + "\\" + PowFuncs[ii].name + "," + InvFuncs[jj].name + ".csv") == false)
                        {
                            StreamWriter sw = new StreamWriter(date + "\\" + PowFuncs[ii].name + "," + InvFuncs[jj].name + ".csv");
                            sw.Write(s);
                            sw.Close();
                        }
                        else
                        {
                            int k = 0;
                            for (; k < s.Length; k++)
                                if (s[k] == '\n')
                                    break;                            
                            s = s.Remove(0, k + 2);
                            if (ii == 19 || ii == 20)
                                s = s.Remove(0, 7);

                            StreamWriter sw = File.AppendText(date + "\\" + PowFuncs[ii].name + "," + InvFuncs[jj].name + ".csv");
                            sw.Write(s);
                            sw.Close();
                        }

                        Action action = () =>
                        {
                            listBox4.Items.Add(PowFuncs[ii].name + "," + InvFuncs[jj].name + " done");
                            listBox4.Update();
                        };
                        Invoke(action);

                        if (act.Value == false)
                        {
                            StreamWriter swt = new StreamWriter("Temp\\Inv.txt");
                            swt.WriteLine(j);
                            for (int k = 0; k < iinds.Count; k++)
                                swt.WriteLine(iinds[k]);
                            swt.Close();

                            break;
                        }
                        wstart = nstart = 0;
                    }
                }
                else
                {
                    s = func1(ii, wstart, astart, bstart, nstart, pols, N, W, A, B, act, PowFuncs[ii], InvFuncs[1]);
                    if (File.Exists(date + "\\" + PowFuncs[ii].name + ".csv") == false)
                    {
                        StreamWriter sw = new StreamWriter(date + "\\" + PowFuncs[ii].name + ".csv");
                        sw.Write(s);
                        sw.Close();
                    }
                    else
                    {                        
                        int k = 0;
                        for (; k < s.Length; k++)
                            if (s[k] == '\n')
                                break;
                        s = s.Remove(0, k + 2);
                                                
                        StreamWriter sw = File.AppendText(date + "\\" + PowFuncs[ii].name + ".csv");
                        sw.Write(s);
                        sw.Close();
                    }

                    Action action = () =>
                    {
                        listBox4.Items.Add(PowFuncs[ii].name+ " done");
                        listBox4.Update();
                    };
                    Invoke(action);

                    if (act.Value == false)
                    {
                        StreamWriter swt = new StreamWriter("Temp\\Inv.txt");
                        swt.WriteLine(0);
                        for (int k = 0; k < iinds.Count; k++)
                            swt.WriteLine(iinds[k]);
                        swt.Close();
                    }
                }

                if (act.Value == false)
                {
                    StreamWriter swt = new StreamWriter("Temp\\Pols.txt");
                    swt.WriteLine(pols[0].mod);
                    for (int k = 0; k < pols.Count(); k++)
                    {
                        if (k != pols.Count - 1)
                            swt.WriteLine(pols[k].ToString());
                        else
                            swt.Write(pols[k].ToString());                        
                    }
                    swt.Close();

                    swt = new StreamWriter("Temp\\Pow.txt");
                    swt.WriteLine(date);
                    swt.WriteLine(i);
                    for (int k = 0; k < pinds.Count; k++)
                        swt.WriteLine(pinds[k]);
                    swt.Close();

                    Application.Exit();
                    break;                    
                }

            }            
        }
        private string func1(int ii, int wstart, int astart, int bstart, int nstart, List<cpolinom> pols, List<BigInteger> N, List<int> W, List<int> A, List<int> B, flag f, MPower pow, MInverse inv)
        {
            string s = "";
            for (int i = 0; i < N.Count; i++)
                s = s + ";" + N[i];            

            if (isWindow(ii) == true)
            {
                for (int w = wstart; w < W.Count; w++)
                {
                    s = s + "\n";
                    s = s + W[w] + ";";
                    s = s + func2(nstart, pols, N, W[w], 0, 0, f, pow, inv);                    
                    nstart = 0;
                    if (act.Value == false)
                    {
                        StreamWriter sw = new StreamWriter("Temp\\B.txt");
                        sw.WriteLine(0);
                        for (int j = 0; j < B.Count; j++)
                            sw.WriteLine(B[j]);
                        sw.Close();

                        sw = new StreamWriter("Temp\\A.txt");
                        sw.WriteLine(0);
                        for (int j = 0; j < A.Count; j++)
                            sw.WriteLine(A[j]);
                        sw.Close();

                        sw = new StreamWriter("Temp\\W.txt");
                        sw.WriteLine(w);
                        for (int j = 0; j < W.Count; j++)
                            sw.WriteLine(W[j]);
                        sw.Close();

                        break;
                    }
                }
            }
            else if (ii == 19 || ii == 20)
            {
                int ast = astart;
                int bst = bstart;

                for (int a = ast; a < A.Count; a++)
                {
                    for (int b = bst; b < B.Count; b++)
                    {
                        s = s + "\n";
                        s = s + "A:" + A[a] + ",B:" + B[b] + ";";
                        s = s + func2(nstart, pols, N, 0, A[a], B[b], f, pow, inv);                        
                        nstart = 0;

                        if (act.Value == false)
                        {
                            StreamWriter sw = new StreamWriter("Temp\\B.txt");
                            sw.WriteLine(b);
                            for (int j = 0; j < B.Count; j++)
                                sw.WriteLine(B[j]);
                            sw.Close();

                            sw = new StreamWriter("Temp\\A.txt");
                            sw.WriteLine(a);
                            for (int j = 0; j < A.Count; j++)
                                sw.WriteLine(A[j]);
                            sw.Close();

                            sw = new StreamWriter("Temp\\W.txt");
                            sw.WriteLine(0);
                            for (int j = 0; j < W.Count; j++)
                                sw.WriteLine(W[j]);
                            sw.Close();

                            break;
                        }                        
                    }
                    bst = 0;
                    if (act.Value == false)                    
                        break;                    
                }
                ast = 0;
            }
            else
            {
                s = s + "\n" + ";";
                s = s + func2(nstart, pols, N, 0, 0, 0, f, pow, inv);
                nstart = 0;
                if (act.Value == false)
                {
                    StreamWriter sw = new StreamWriter("Temp\\B.txt");
                    sw.WriteLine(0);
                    for (int j = 0; j < B.Count; j++)
                        sw.WriteLine(B[j]);
                    sw.Close();

                    sw = new StreamWriter("Temp\\A.txt");
                    sw.WriteLine(0);
                    for (int j = 0; j < A.Count; j++)
                        sw.WriteLine(A[j]);
                    sw.Close();

                    sw = new StreamWriter("Temp\\W.txt");
                    sw.WriteLine(0);
                    for (int j = 0; j < W.Count; j++)
                        sw.WriteLine(W[j]);
                    sw.Close();
                }
            }
            return s;
        }
        private string func2(int nstart, List<cpolinom> pols, List<BigInteger> N, int w, int a, int b, flag f, MPower pow, MInverse inv)
        {
            string s = "";
            for (int i = nstart; i < N.Count; i++)
            {
                int sum = 0;
                Stopwatch stw = new Stopwatch();

                for (int j = 1; j < pols.Count; j++)
                {
                    List<cpolinom> temp = new List<cpolinom>();
                    temp.Add(pols[0]);
                    temp.Add(pols[j]);

                    stw.Start();
                    pow.pow(pols, N[i], w, a, b, inv.inv);
                    stw.Stop();

                    sum += (int)stw.Elapsed.TotalMilliseconds;
                    stw.Reset();
                }
                double res = sum / (pols.Count - 1);
                s = s + res.ToString() + ";";

                if (act.Value == false)
                {
                    Action action = () =>
                    {
                        listBox4.Items.Add("Stoped");
                        listBox4.Update();
                    };
                    Invoke(action);

                    StreamWriter sw = new StreamWriter("Temp\\N.txt");
                    sw.WriteLine(i + 1);
                    for (int j = 0; j < N.Count; j++)
                        sw.WriteLine(N[j]);
                    sw.Close();

                    break;
                }
            }
            return s;
        }
        private void WriteInfo(List<BigInteger> N, List<int> W, List<int> A, List<int> B, string fname)
        {
            StreamWriter sw = new StreamWriter(fname);

            sw.Write("N: ");
            sw.Write(N[0]);
            for (int i = 1; i < N.Count; i++)
                sw.Write(", " + N[i]);
            sw.WriteLine("");

            sw.Write("W: ");
            sw.Write(W[0]);
            for (int i = 1; i < W.Count; i++)
                sw.Write(", " + W[i]);
            sw.WriteLine("");

            sw.Write("A: ");
            sw.Write(A[0]);
            for (int i = 1; i < A.Count; i++)
                sw.Write(", " + A[i]);
            sw.WriteLine("");

            sw.Write("B: ");
            sw.Write(B[0]);
            for (int i = 1; i < B.Count; i++)
                sw.Write(", " + B[i]);
            sw.WriteLine("");

            sw.Close();
        }

        private void button8_Click(object sender, EventArgs e)
        {
            int p = Convert.ToInt32(textBox5.Text);
            BigInteger N = BigInteger.Parse(textBox4.Text);
            int w = Convert.ToInt32(textBox9.Text);
            int A = Convert.ToInt32(textBox13.Text);
            int B = Convert.ToInt32(textBox14.Text);
            cpolinom c = new cpolinom(textBox6.Text, p);
            cpolinom ir = new cpolinom(textBox8.Text, p);
            List<string> s = new List<string>();
            listBox4.Items.Clear();

            if (checkedListBox1.CheckedIndices.Count > 0)
            {
                for (int f = 0; f < checkedListBox3.CheckedIndices.Count; f++)
                {
                    int ff = checkedListBox3.CheckedIndices[f];
                    if (p > 2 && (ff > 4 && ff < 8))
                        continue;
                    int count = checkedListBox1.Items.Count;
                    for (int i = 0; i < checkedListBox1.CheckedIndices.Count; i++)
                    {
                        int j = checkedListBox1.CheckedIndices[i];
                        List<cpolinom> pols = new List<cpolinom>();
                        pols.Add(ir);
                        pols.Add(c);

                        int ind = j + ff * count;
                        if (isNaf(j) == true)
                        {
                            for (int k = 0; k < checkedListBox2.CheckedIndices.Count; k++)
                            {
                                int jk = checkedListBox2.CheckedIndices[k];
                                listBox4.Items.Add(PowFuncs[ind].name + ", " + InvFuncs[jk].name + ": " + (PowFuncs[ind].pow(pols, N, w, A, B, InvFuncs[jk].inv)).ToString());
                            }
                        }
                        else
                            listBox4.Items.Add(PowFuncs[ind].name + ": " + (PowFuncs[ind].pow(pols, N, w, A, B, InvFuncs[0].inv)).ToString());

                        listBox4.Update();
                    }
                }
            }
        }
        
        private void button9_Click(object sender, EventArgs e)
        {
            for (int i = 0; i < checkedListBox1.Items.Count; i++)
                checkedListBox1.SetItemChecked(i, true);
        }
        
        private void button10_Click(object sender, EventArgs e)
        {
            for (int i = 0; i < checkedListBox1.Items.Count; i++)
                checkedListBox1.SetItemChecked(i, false);
        }        

        private void button11_Click(object sender, EventArgs e)
        {
            act.Value = true;
            listBox4.Items.Clear();

            StreamReader sr = new StreamReader("Temp\\Pow.txt");
            string date = sr.ReadLine();
            int oldPow = Convert.ToInt32(sr.ReadLine());
            List<int> PowInds = new List<int>();
            while (sr.EndOfStream != true)
                PowInds.Add(Convert.ToInt32(sr.ReadLine()));
            sr.Close();

            sr = new StreamReader("Temp\\Inv.txt");
            int oldInv = Convert.ToInt32(sr.ReadLine());
            List<int> InvInds = new List<int>();
            while (sr.EndOfStream != true)
                InvInds.Add(Convert.ToInt32(sr.ReadLine()));
            sr.Close();

            sr = new StreamReader("Temp\\W.txt");
            int iW = Convert.ToInt32(sr.ReadLine());
            List<int> W = new List<int>();
            while (sr.EndOfStream != true)
                W.Add(Convert.ToInt32(sr.ReadLine()));
            sr.Close();

            sr = new StreamReader("Temp\\A.txt");
            int iA = Convert.ToInt32(sr.ReadLine());
            List<int> A = new List<int>();
            while (sr.EndOfStream != true)
                A.Add(Convert.ToInt32(sr.ReadLine()));
            sr.Close();

            sr = new StreamReader("Temp\\B.txt");
            int iB = Convert.ToInt32(sr.ReadLine());
            List<int> B = new List<int>();
            while (sr.EndOfStream != true)
                B.Add(Convert.ToInt32(sr.ReadLine()));
            sr.Close();

            sr = new StreamReader("Temp\\N.txt");
            int iN = Convert.ToInt32(sr.ReadLine());
            List<BigInteger> N = new List<BigInteger>();
            while (sr.EndOfStream != true)
                N.Add(BigInteger.Parse(sr.ReadLine()));
            sr.Close();

            sr = new StreamReader("Temp\\Pols.txt");
            int mod = Convert.ToInt32(sr.ReadLine());
            List<cpolinom> pols = new List<cpolinom>();
            while (sr.EndOfStream != true)
                pols.Add(new cpolinom(sr.ReadLine(), mod));
            sr.Close();

            args a = new args(pols, PowInds, InvInds, W, A, B, N, oldPow, oldInv, iW, iA, iB, iN, date);

            Thread th = new Thread(Pows);
            th.Start(a);
        }
        
        private void button12_Click(object sender, EventArgs e)
        {
            act.Value = false;
            listBox4.Items.Add("Stop button was clicked. Wait...");
        }

        private void button14_Click(object sender, EventArgs e)
        {
            for (int i = 0; i < checkedListBox2.Items.Count; i++)
                checkedListBox2.SetItemChecked(i, true);
        }

        private void button13_Click(object sender, EventArgs e)
        {
            for (int i = 0; i < checkedListBox2.Items.Count; i++)
                checkedListBox2.SetItemChecked(i, false);
        }

        private void button15_Click(object sender, EventArgs e)
        {
            int p = Convert.ToInt32(textBox5.Text);
            cpolinom c = new cpolinom(textBox6.Text, p);
            cpolinom ir = new cpolinom(textBox8.Text, p);
            List<string> s = new List<string>();
            listBox4.Items.Clear();

            if (checkedListBox2.CheckedIndices.Count > 0)
            {

                for (int i = 0; i < checkedListBox2.CheckedIndices.Count; i++)
                {
                    int j = checkedListBox2.CheckedIndices[i];

                    listBox4.Items.Add(InvFuncs[j].name + ": " + (InvFuncs[j].inv(c, ir).ToString()));

                    listBox4.Update();
                }

            }
        }

        private void button17_Click(object sender, EventArgs e)
        {
            for (int i = 0; i < checkedListBox3.Items.Count; i++)
                checkedListBox3.SetItemChecked(i, true);
        }

        private void button16_Click(object sender, EventArgs e)
        {
            for (int i = 0; i < checkedListBox3.Items.Count; i++)
                checkedListBox3.SetItemChecked(i, false);
        }

        private void button21_Click(object sender, EventArgs e)
        {
            for (int i = 0; i < checkedListBox5.Items.Count; i++)
                checkedListBox5.SetItemChecked(i, true);
        }

        private void button20_Click(object sender, EventArgs e)
        {
            for (int i = 0; i < checkedListBox5.Items.Count; i++)
                checkedListBox5.SetItemChecked(i, false);
        }

        private void button19_Click(object sender, EventArgs e)
        {
            for (int i = 0; i < checkedListBox4.Items.Count; i++)
                checkedListBox4.SetItemChecked(i, true);
        }

        private void button18_Click(object sender, EventArgs e)
        {
            for (int i = 0; i < checkedListBox4.Items.Count; i++)
                checkedListBox4.SetItemChecked(i, false);
        }

        private void button23_Click(object sender, EventArgs e)
        {
            for (int i = 0; i < checkedListBox6.Items.Count; i++)
                checkedListBox6.SetItemChecked(i, true);
        }

        private void button22_Click(object sender, EventArgs e)
        {
            for (int i = 0; i < checkedListBox6.Items.Count; i++)
                checkedListBox6.SetItemChecked(i, false);
        }

        private void button24_Click(object sender, EventArgs e)
        {
            string s = listBox6.Items[listBox6.SelectedIndex].ToString();
            int n = FindN(s);

            cpolinom pol = new cpolinom("", 2);


            pol = GetPolinom(n);

            StreamWriter w = new StreamWriter("2," + n.ToString() + ".txt");
            pol.PrintPolinom(w);
            w.Close();

            System.Windows.Forms.MessageBox.Show("Done");
        }
        private cpolinom GetPolinom(int n)
        {
            cpolinom x1 = new cpolinom(1, 2);
            cpolinom x2 = new cpolinom(2, 2);
            cpolinom x3 = new cpolinom(3, 2);
            cpolinom x4 = new cpolinom(4, 2);
            cpolinom x5 = new cpolinom(5, 2);
            cpolinom x6 = new cpolinom(6, 2);
            cpolinom x7 = new cpolinom(7, 2);
            cpolinom x8 = new cpolinom(8, 2);
            cpolinom x9 = new cpolinom(9, 2);
            cpolinom x10 = new cpolinom(10, 2);
            cpolinom x13 = new cpolinom(13, 2);
            cpolinom x14 = new cpolinom(14, 2);
            cpolinom x15 = new cpolinom(15, 2);
            cpolinom x19 = new cpolinom(19, 2);
            cpolinom x27 = new cpolinom(27, 2);

            cpolinom r = new cpolinom("1", 2);
            cpolinom h = new cpolinom(n, 2);
            if (n == 2) r = h + x1 + r;
            if (n == 4) r = h + x1 + r;
            if (n == 8) r = h + x4 + x3 + x1 + r;
            if (n == 16) r = h + x5 + x3 + x1 + r;
            if (n == 32) r = h + x7 + x3 + x2 + r;
            if (n == 64) r = h + x6 + x3 + x1 + r;
            if (n == 128) r = h + x7 + x2 + x1 + r;
            if (n == 256) r = h + x10 + x5 + x2 + r;
            if (n == 512) r = h + x8 + x5 + x2 + r;
            if (n == 1024) r = h + x19 + x6 + x1 + r;
            if (n == 2048) r = h + x19 + x14 + x13 + r;
            if (n == 4096) r = h + x27 + x15 + x1 + r;
            if (n == 8192) r = h + x9 + x5 + x2 + r;

            return r;
        }
    }
    

    delegate cpolinom Power(List<cpolinom> pols, BigInteger n, int w, int A, int B, Inverse Inv);
    delegate cpolinom Oper(cpolinom a, cpolinom b);

    class MPower
    {
        public Power pow;
        public string name;

        public MPower(Power p, string n)
        {
            pow = p;
            name = n;
        }
    }
    class MInverse
    {
        public Inverse inv;
        public string name;

        public MInverse(Inverse i, string n)
        {
            inv = i;
            name = n;
        }
    }
    class MMulti
    {
        public Multi mul;
        public string name;

        public MMulti(Multi i, string n)
        {
            mul = i;
            name = n;
        }
    }
    class MOperation
    {
        public Oper op;
        public string name;

        public MOperation(Oper o, string s)
        {
            op = o;
            name = s;
        }
    }

    public class args
    {
        public List<cpolinom> pols;
        public List<int> PowInds;
        public List<int> InvInds;
        public List<int> W;
        public List<int> A;
        public List<int> B;
        public List<BigInteger> N;
        public int pstart;
        public int istart;
        public int wstart;
        public int astart;
        public int bstart;
        public int nstart;
        public string date;

        public args(List<cpolinom> pol, List<int> pi, List<int> ii, List<int> w, List<int> a, List<int> b, List<BigInteger> n, int ip, int iS, int ws, int As, int bs, int ns, string d)
        {
            pols = pol;
            PowInds = pi;
            InvInds = ii;
            W = w;
            A = a;
            B = b;
            N = n;
            pstart = ip;
            istart = iS;
            wstart = ws;
            astart = As;
            bstart = bs;
            nstart = ns;
            date = d;
        }
    }
    public class flag
    {
        private bool fl;
        public bool Value
        {
            set { fl = value; }
            get { return fl; }
        }
    }
}
