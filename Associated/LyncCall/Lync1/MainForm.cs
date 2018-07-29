/* HEADER: --------------------------------------------------------------------------------------------------------------------------------------------------- *
 *                                                                                                                                                             *
 *  Name:             Lync Call Application for Unity for Debt Management                                                                                      *
 *  Version:          0.1                                                                                                                                      *
 *  (C)(R):           Tomasz Kandula                                                                                                                           *
 *  Originate:        27-12-2017                                                                                                                               *
 *  IDE:              Visual Studio 2013                                                                                                                       *
 *  Target:           Microsoft Windows 7 or newer                                                                                                             *
 *  NET Framework:    Required 4.5 or newer                                                                                                                    *
 *  LYNC version:     2013 or newer                                                                                                                            *
 *  Updated:          28-12-2017                                                                                                                               *
 *                                                                                                                                                             *
 * ----------------------------------------------------------------------------------------------------------------------------------------------------------- */

/* ADDITIONAL DOCUMENTATION: --------------------------------------------------------------------------------------------------------------------------------- *
 *                                                                                                                                                             *
 * Lync Call automation: https://msdn.microsoft.com/en-us/library/office/jj933146.aspx                                                                         *
 * ModalityState enumeration: https://msdn.microsoft.com/en-us/library/microsoft.lync.model.conversation.modalitystate_di_3_uc_ocs14mreflyncclnt               *
 *                                                                                                                                                             *
 * ----------------------------------------------------------------------------------------------------------------------------------------------------------- */

/* ----------------------------------------------------------------------------------------------------------------------------------------- SYSTEM ASSEMBLIES */
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Runtime.InteropServices;

/* ---------------------------------------------------------------------------------------------------------------------------------- LYNC SDK 2013 ASSEMBLIES */
using Microsoft.Lync.Model;
using Microsoft.Lync.Model.Conversation;
using Microsoft.Lync.Model.Conversation.AudioVideo;
using Microsoft.Lync.Model.Extensibility;

/* -------------------------------------------------------------------------------------------------------------------------------------------- MAIN NAMESPACE */
namespace nsLyncCall
{
    public partial class MainForm: Form
    {

        /* --------------------------------------------------------------- ! GENERAL ! ----------------------------------------------------------------------- */

        static       int    TotalTimeSec     = 0;
        public const int    WM_NCLBUTTONDOWN = 0xA1;
        public const int    HT_CAPTION       = 0x2;
        static       string CLASS_NAME       = "TMainForm";                    /* TO SEND MESSAGE TO ANOTHER APPLICATION, WE USE CLASS NAME OF ITS WINDOW FORM */
        public const int    WM_APP           = 0x8000;                         /* APPLICATION DEFINED MESSAGES STARTS FROM $8000 TO $BFFF, USE AS BASE NUMBER  */
        static       int    WM_EXTINFO       = WM_APP + 150;                   /* WE ADD CONSTANT, THE SAME NUMBER MUST BE USE IN ANOTHER APPLICATION          */

        /* -------------------------------------------------------------- ! DLL IMPORTS ! -------------------------------------------------------------------- */

        [DllImportAttribute("user32.dll")]
        public static extern int SendMessage(IntPtr hWnd, int Msg, int wParam, int lParam);
        [DllImportAttribute("user32.dll")]
        public static extern IntPtr FindWindow(string lpClassName, String lpWindowName);
        [DllImportAttribute("user32.dll")]
        static extern bool IsWindow(IntPtr hWnd);
        [DllImportAttribute("user32.dll")]
        public static extern bool ReleaseCapture();
        
        /* --------------------------------------------------- ! WINDOW SHADOW FOR BORDERLESS FORM ! --------------------------------------------------------- */
        protected override CreateParams CreateParams
        {
            get
            {
                const int CS_DROPSHADOW = 0x20000;
                CreateParams cp = base.CreateParams;
                cp.ClassStyle |= CS_DROPSHADOW;
                return cp;
            }
        }

        /* -------------------------------------------------------- ! MAIN FORM INITIALIZATION ! ------------------------------------------------------------- */
        public MainForm()
        {
            InitializeComponent();
        }

        /* ----------------------------------------------------------- ! APPLICATION EVENTS ! ---------------------------------------------------------------- */

        /* ----------------------------------------------------------------------------------------------------------------------------------------- ON CREATE */
        private void MainForm_Load(object sender, EventArgs e)
        {
            Number_Label.Text  = "";
            Status_Label.Text  = "";
            Time_Label.Text    = "";
            CallTimer.Enabled  = false;
            CheckTimer.Enabled = false;
        }

        /* -------------------------------------------------------------------------------------------------------------------------------------- ON FORM SHOW */
        private void MainForm_Shown(object sender, EventArgs e)
        {
            /* INITIALIZE */
            string[] args = Environment.GetCommandLineArgs();
            string Parameter = "";
            string StrNumber = "";
            long   IntNumber = 0;

            /* READ THE FIRST GIVEN APPLICATION ARGUMENT */
            try
            { 
                Parameter = args[1]; 
            }
            catch
            {
                Parameter = "";
            }

            /* CALL THE GIVEN PHONE NUMBER */
            if (Parameter != "") 
            {
                /* CHECK IF ARGUMENT IS A NUMBER | STRING TO INTEGER */
                if (Int64.TryParse(Parameter, out IntNumber))
                {
                    StrNumber = "tel:+" + Parameter;
                    Number_Label.Text = "+" + Parameter;
                    CallViaLync(StrNumber);
                    CheckTimer.Enabled = true;
                }
                else
                {
                    Number_Label.Text = Parameter;
                    Status_Label.Text = "Incorrect phone number.";
                }
            }
            else
            {
                Number_Label.Text = "+0 000 000 000";
                Status_Label.Text = "No phone number given.";
            }
        }

        /* ------------------------------------------------------------------------------------------------------------------------------------ ON CLOSE QUERY */
        private void MainForm_FormClosing(object sender, FormClosingEventArgs e)
        {
            /* SEND MESSAGE TO UNITY APPLICATION */
            IntPtr HWND = FindWindow(CLASS_NAME, null);
            if (IsWindow(HWND) == true)
            {
                SendMessage(HWND, WM_EXTINFO, 14, TotalTimeSec);
            }

            /* ALLOW TO QUIT */
            e.Cancel = false;
        }

        /* --------------------------------------------------------------- ! MOUSE EVENTS ! ------------------------------------------------------------------ */

        /* --------------------------------------------------------------------------------------------------------------------------------- CLOSE APPLICATION */
        private void CloseImage_Click(object sender, EventArgs e)
        {
            Application.Exit();
        }
        
        /* ----------------------------------------------------------------------------------------------------------------------------------- MAIN FORM CLICK */
        private void MainForm_MouseDown(object sender, MouseEventArgs e)
        {
            /* MOVE FORM WHEN LEFT MOUSE BUTTON IS CLICKED */
            if (e.Button == MouseButtons.Left)
            {
                ReleaseCapture();
                SendMessage(Handle, WM_NCLBUTTONDOWN, HT_CAPTION, 0);
            }
        }

        /* --------------------------------------------------------------------------------------------------------------------------------------- PANEL CLICK */
        private void MainPanel_MouseDown(object sender, MouseEventArgs e)
        {
            /* MOVE FORM WHEN LEFT MOUSE BUTTON IS CLICKED */
            if (e.Button == MouseButtons.Left)
            {
                ReleaseCapture();
                SendMessage(Handle, WM_NCLBUTTONDOWN, HT_CAPTION, 0);
            }
        }

        /* ------------------------------------------------------------- ! APPLICATION TIMERS ! -------------------------------------------------------------- */

        /* ----------------------------------------------------------------------------------------------------------------------------------------- ON TIMERS */

        /* CHECK IF CALL STARTED */
        private void CheckTimer_Tick(object sender, EventArgs e)
        {      
            /* ASSIGN CURRENT STATUS */
            Status_Label.Text = GetLyncState();
   
            /* CHANGE STATUS */
            if (Status_Label.Text == "In a call...")
            {
                CallTimer.Enabled = true;
            }
            else
            {
                CallTimer.Enabled = false;
            }

            /* CLOSE PROGRAM IF CALL HAS ENDED */
            if (CallTimer.Enabled == false && Time_Label.Text != "") 
            {
                Application.Exit();
            }
        }

        /* MEASURE CALL TIME */
        private void CallTimer_Tick(object sender, EventArgs e)
        {
            TotalTimeSec = TotalTimeSec + 1;
            Time_Label.Text = TotalTimeSec.ToString() + " seconds.";
        }

        /* ---------------------------------------------------------- ! DESIGNED METHODS ! ------------------------------------------------------------------- */
        
        /* RETURN LYNC STATE: BUSY OR FREE LINE */
        public string GetLyncState()
        {
            var result = 0; /* DEFAULT FREE */
            LyncClient lyncClient = LyncClient.GetClient();
            foreach (Conversation UserCallState in lyncClient.ConversationManager.Conversations)
            {
                if (UserCallState.State == ConversationState.Active)
                {
                    if (((AVModality)UserCallState.Modalities[ModalityTypes.AudioVideo]).State == ModalityState.Connected)
                    {
                        result = 1; /* IN A CALL */
                        break;
                    }
                }
            }
            if (result == 1) { return "In a call..."; } else { return "Line is free..."; };
        }

        /* LYNC CALL METHOD */
        public Boolean CallViaLync(string TelNumber)
        {
            LyncClient lyncClient = LyncClient.GetClient();

            var result = false;                            /* FAIL TO CALL BY DEFAULT */
            var automation = LyncClient.GetAutomation();   /* AUTOMATION OBJECT       */

            List<string> participants = new List<string>(1);
            participants.Add(TelNumber);

            try
            {
                automation.BeginStartConversation(AutomationModalities.Audio, participants, null, null, automation);
                result = true;
            }
            catch (LyncClientException lyncClientException)
            {
                MessageBox.Show("Cannot make a call. Error thrown: " + lyncClientException + ". Please contact IT support.");
                result = false;
            }
            return result;
        }
    
    /* MAIN FORM CLASS ENDS */
    } 

/* END */
}
