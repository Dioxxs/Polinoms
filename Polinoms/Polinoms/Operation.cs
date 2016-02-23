using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
namespace Polinoms
{
    abstract class Operation
    {
        public virtual string Name() { return ""; }
        public virtual void LoopFunc(List<cpolinom> pols) { }
        public virtual void Result() { }
        public virtual float Func(Stopwatch stw, List<cpolinom> pols) { return 0; }
        public void oper(string s, int p)
        {
            Stopwatch stw = new Stopwatch();
            StreamReader sr = new StreamReader(s);
            StreamWriter sw = new StreamWriter(Name());
            List<cpolinom> pols = new List<cpolinom>();

            string str = sr.ReadLine();
            str = str.Substring(1, str.Length - 2);
            pols.Add(new cpolinom(str, p));
            sr.ReadLine();
            pols.Add(new cpolinom(sr.ReadLine(), p));

            float sum = 0;
            int k = 0;
            for (; sr.EndOfStream == false; k++)
            {
                pols.Add(new cpolinom(sr.ReadLine(), p));
                if (k == 0)
                    LoopFunc(pols);

                stw.Start();
                LoopFunc(pols);
                stw.Stop();

                sw.WriteLine((float)(stw.ElapsedTicks) / 10);
                sum += (float)(stw.ElapsedTicks / 10);

                pols.RemoveAt(1);
                stw.Reset();
            }
            float f = Func(stw, pols);
            if (f != 0)
            {
                k++;
                sw.WriteLine(f);
                sum += f;
            }

            sw.WriteLine("");
            sw.WriteLine("Whole time:" + sum);
            sw.Write("Averange time:" + (float)sum / k);

            sw.Close();
            sr.Close();

            Result();
        }
    }
    class Oper_Add : Operation
    {
        public override string Name() { return "AddtionResults.txt"; }
        public override void LoopFunc(List<cpolinom> pols) { cpolinom e = (pols[2] + pols[1]) % pols[0]; }
        public override void Result() { System.Windows.Forms.MessageBox.Show("Addition done"); }
    }
    class Oper_Sub : Operation
    {
        public override string Name() { return "SubscriptionResults.txt"; }
        public override void LoopFunc(List<cpolinom> pols) { cpolinom e = (pols[2] - pols[1]) % pols[0]; }
        public override void Result() { System.Windows.Forms.MessageBox.Show("Subscription done"); }
    }
    class Oper_Mul : Operation
    {
        public override string Name() { return "MultiplyResults.txt"; }
        public override void LoopFunc(List<cpolinom> pols) { cpolinom e = (pols[2] * pols[1]) % pols[0]; }
        public override void Result() { System.Windows.Forms.MessageBox.Show("Multiply done"); }
    }
    class Oper_Div : Operation
    {
        public override string Name() { return "DivisionResults.txt"; }
        public override void LoopFunc(List<cpolinom> pols) { cpolinom e = (pols[2] / pols[1]) % pols[0]; }
        public override void Result() { System.Windows.Forms.MessageBox.Show("Division done"); }
    }
    class Oper_Mod : Operation
    {
        public override string Name() { return "ModResults.txt"; }
        public override void LoopFunc(List<cpolinom> pols) { cpolinom e = pols[2] % pols[1]; }
        public override void Result() { System.Windows.Forms.MessageBox.Show("Mod done"); }
    }
    class Oper_Inv : Operation
    {
        public override string Name() { return "InvResults.txt"; }
        public override void LoopFunc(List<cpolinom> pols) { cpolinom e = pols[1].InvPolinom(pols[0]); }
        public override void Result() { System.Windows.Forms.MessageBox.Show("Inverse done"); }
        public override float Func(Stopwatch stw, List<cpolinom> pols)
        {
            stw.Start();
            LoopFunc(pols);
            stw.Stop();
            return (float)(stw.ElapsedTicks) / 10;
        }
    }
    class Oper_Evk : Operation
    {
        public override string Name() { return "EvkInvResults.txt"; }
        public override void LoopFunc(List<cpolinom> pols) { cpolinom e = pols[1].EvkPolinom(pols[0]); }
        public override void Result() { System.Windows.Forms.MessageBox.Show("Evklid inverse done"); }
        public override float Func(Stopwatch stw, List<cpolinom> pols)
        {
            stw.Start();
            LoopFunc(pols);
            stw.Stop();
            return (float)(stw.ElapsedTicks) / 10;
        }
    }
    class Oper_Inv2 : Operation
    {
        public override string Name() { return "Inv2Results.txt"; }
        public override void LoopFunc(List<cpolinom> pols) { cpolinom e = pols[1].Inv2Polinom(pols[0]); }
        public override void Result() { System.Windows.Forms.MessageBox.Show("Inverse2 done"); }
        public override float Func(Stopwatch stw, List<cpolinom> pols)
        {
            stw.Start();
            LoopFunc(pols);
            stw.Stop();
            return (float)(stw.ElapsedTicks) / 10;
        }
    }
}
