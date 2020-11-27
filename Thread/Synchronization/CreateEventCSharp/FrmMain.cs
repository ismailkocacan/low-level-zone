using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading;
using System.Windows.Forms;

namespace CreateEvents
{
    public partial class FrmMain : Form
    {
        public FrmMain()
        {
            InitializeComponent();
        }

        void ParameterizedThreadStartProc(object obj)
        {
            EventWaitHandle eventWaitHandle = (EventWaitHandle)obj;
            while (true)
            {
                if (eventWaitHandle.WaitOne())
                {
                    Debug.WriteLine(">>>>>>>>>>>> Otobüs geldi.");
                    eventWaitHandle.Reset();
                }
            }
        }

        private void btnCreateEvent_Click(object sender, EventArgs e)
        {
            EventWaitHandle eventHandle = new EventWaitHandle(false, EventResetMode.ManualReset, "MyEvent");
            ParameterizedThreadStart functionPointer = ParameterizedThreadStartProc;
            Thread thread = new Thread(functionPointer, 0);
            thread.Start(eventHandle);
        }

        private void btnSetEvent_Click(object sender, EventArgs e)
        {
            EventWaitHandle eventWaitHandle = EventWaitHandle.OpenExisting("MyEvent");
            eventWaitHandle.Set();
        }
    }
}
