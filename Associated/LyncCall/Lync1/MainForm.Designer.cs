namespace nsLyncCall
{
    partial class MainForm
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
            this.CallText = new System.Windows.Forms.Label();
            this.StatusText = new System.Windows.Forms.Label();
            this.CallTimer = new System.Windows.Forms.Timer(this.components);
            this.CheckTimer = new System.Windows.Forms.Timer(this.components);
            this.CallTimeText = new System.Windows.Forms.Label();
            this.MainPanel = new System.Windows.Forms.Panel();
            this.CloseImage = new System.Windows.Forms.PictureBox();
            this.Time_Label = new System.Windows.Forms.Label();
            this.Status_Label = new System.Windows.Forms.Label();
            this.Number_Label = new System.Windows.Forms.Label();
            this.LyncImage = new System.Windows.Forms.PictureBox();
            this.MainPanel.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.CloseImage)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.LyncImage)).BeginInit();
            this.SuspendLayout();
            // 
            // CallText
            // 
            this.CallText.AutoSize = true;
            this.CallText.Font = new System.Drawing.Font("Microsoft Sans Serif", 8F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(238)));
            this.CallText.ForeColor = System.Drawing.Color.Black;
            this.CallText.Location = new System.Drawing.Point(94, 15);
            this.CallText.Name = "CallText";
            this.CallText.Size = new System.Drawing.Size(47, 13);
            this.CallText.TabIndex = 1;
            this.CallText.Text = "Number:";
            // 
            // StatusText
            // 
            this.StatusText.AutoSize = true;
            this.StatusText.Font = new System.Drawing.Font("Microsoft Sans Serif", 8F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(238)));
            this.StatusText.ForeColor = System.Drawing.Color.Black;
            this.StatusText.Location = new System.Drawing.Point(94, 36);
            this.StatusText.Name = "StatusText";
            this.StatusText.Size = new System.Drawing.Size(58, 13);
            this.StatusText.TabIndex = 3;
            this.StatusText.Text = "Call status:";
            // 
            // CallTimer
            // 
            this.CallTimer.Interval = 1000;
            this.CallTimer.Tick += new System.EventHandler(this.CallTimer_Tick);
            // 
            // CheckTimer
            // 
            this.CheckTimer.Interval = 500;
            this.CheckTimer.Tick += new System.EventHandler(this.CheckTimer_Tick);
            // 
            // CallTimeText
            // 
            this.CallTimeText.AutoSize = true;
            this.CallTimeText.Font = new System.Drawing.Font("Microsoft Sans Serif", 8F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(238)));
            this.CallTimeText.ForeColor = System.Drawing.Color.Black;
            this.CallTimeText.Location = new System.Drawing.Point(94, 58);
            this.CallTimeText.Name = "CallTimeText";
            this.CallTimeText.Size = new System.Drawing.Size(49, 13);
            this.CallTimeText.TabIndex = 4;
            this.CallTimeText.Text = "Call time:";
            // 
            // MainPanel
            // 
            this.MainPanel.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.MainPanel.Controls.Add(this.CloseImage);
            this.MainPanel.Controls.Add(this.Time_Label);
            this.MainPanel.Controls.Add(this.Status_Label);
            this.MainPanel.Controls.Add(this.Number_Label);
            this.MainPanel.Controls.Add(this.LyncImage);
            this.MainPanel.Controls.Add(this.CallTimeText);
            this.MainPanel.Controls.Add(this.CallText);
            this.MainPanel.Controls.Add(this.StatusText);
            this.MainPanel.Location = new System.Drawing.Point(5, 6);
            this.MainPanel.Name = "MainPanel";
            this.MainPanel.Size = new System.Drawing.Size(456, 91);
            this.MainPanel.TabIndex = 5;
            this.MainPanel.MouseDown += new System.Windows.Forms.MouseEventHandler(this.MainPanel_MouseDown);
            // 
            // CloseImage
            // 
            this.CloseImage.Cursor = System.Windows.Forms.Cursors.Arrow;
            this.CloseImage.Image = global::nsLyncCall.Properties.Resources.BtnClose_16x16;
            this.CloseImage.Location = new System.Drawing.Point(435, 3);
            this.CloseImage.Name = "CloseImage";
            this.CloseImage.Size = new System.Drawing.Size(16, 16);
            this.CloseImage.TabIndex = 9;
            this.CloseImage.TabStop = false;
            this.CloseImage.Click += new System.EventHandler(this.CloseImage_Click);
            // 
            // Time_Label
            // 
            this.Time_Label.AutoSize = true;
            this.Time_Label.Font = new System.Drawing.Font("Microsoft Sans Serif", 8F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(238)));
            this.Time_Label.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(64)))), ((int)(((byte)(64)))), ((int)(((byte)(64)))));
            this.Time_Label.Location = new System.Drawing.Point(175, 58);
            this.Time_Label.Name = "Time_Label";
            this.Time_Label.Size = new System.Drawing.Size(33, 13);
            this.Time_Label.TabIndex = 8;
            this.Time_Label.Text = "{    }";
            // 
            // Status_Label
            // 
            this.Status_Label.AutoSize = true;
            this.Status_Label.Font = new System.Drawing.Font("Microsoft Sans Serif", 8F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(238)));
            this.Status_Label.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(64)))), ((int)(((byte)(64)))), ((int)(((byte)(64)))));
            this.Status_Label.Location = new System.Drawing.Point(175, 36);
            this.Status_Label.Name = "Status_Label";
            this.Status_Label.Size = new System.Drawing.Size(33, 13);
            this.Status_Label.TabIndex = 7;
            this.Status_Label.Text = "{    }";
            // 
            // Number_Label
            // 
            this.Number_Label.AutoSize = true;
            this.Number_Label.Font = new System.Drawing.Font("Microsoft Sans Serif", 8F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(238)));
            this.Number_Label.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(64)))), ((int)(((byte)(64)))), ((int)(((byte)(64)))));
            this.Number_Label.Location = new System.Drawing.Point(175, 15);
            this.Number_Label.Name = "Number_Label";
            this.Number_Label.Size = new System.Drawing.Size(33, 13);
            this.Number_Label.TabIndex = 6;
            this.Number_Label.Text = "{    }";
            // 
            // LyncImage
            // 
            this.LyncImage.Image = global::nsLyncCall.Properties.Resources.Ring64x64;
            this.LyncImage.Location = new System.Drawing.Point(14, 12);
            this.LyncImage.Name = "LyncImage";
            this.LyncImage.Size = new System.Drawing.Size(64, 64);
            this.LyncImage.TabIndex = 5;
            this.LyncImage.TabStop = false;
            // 
            // MainForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(216)))), ((int)(((byte)(232)))), ((int)(((byte)(254)))));
            this.ClientSize = new System.Drawing.Size(467, 103);
            this.Controls.Add(this.MainPanel);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "MainForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Unity - Lync Call";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.MainForm_FormClosing);
            this.Load += new System.EventHandler(this.MainForm_Load);
            this.Shown += new System.EventHandler(this.MainForm_Shown);
            this.MouseDown += new System.Windows.Forms.MouseEventHandler(this.MainForm_MouseDown);
            this.MainPanel.ResumeLayout(false);
            this.MainPanel.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.CloseImage)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.LyncImage)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Label CallText;
        private System.Windows.Forms.Label StatusText;
        private System.Windows.Forms.Timer CallTimer;
        private System.Windows.Forms.Timer CheckTimer;
        private System.Windows.Forms.Label CallTimeText;
        private System.Windows.Forms.Panel MainPanel;
        private System.Windows.Forms.PictureBox LyncImage;
        private System.Windows.Forms.Label Time_Label;
        private System.Windows.Forms.Label Status_Label;
        private System.Windows.Forms.Label Number_Label;
        private System.Windows.Forms.PictureBox CloseImage;
    }
}

