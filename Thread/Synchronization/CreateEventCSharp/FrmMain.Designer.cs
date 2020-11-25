namespace CreateEvents
{
    partial class FrmMain
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
            this.btnCreateEvent = new System.Windows.Forms.Button();
            this.btnSetEvent = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // btnCreateEvent
            // 
            this.btnCreateEvent.Font = new System.Drawing.Font("Tahoma", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(162)));
            this.btnCreateEvent.Location = new System.Drawing.Point(26, 72);
            this.btnCreateEvent.Name = "btnCreateEvent";
            this.btnCreateEvent.Size = new System.Drawing.Size(210, 177);
            this.btnCreateEvent.TabIndex = 0;
            this.btnCreateEvent.Text = "Create Event";
            this.btnCreateEvent.UseVisualStyleBackColor = true;
            this.btnCreateEvent.Click += new System.EventHandler(this.btnCreateEvent_Click);
            // 
            // btnSetEvent
            // 
            this.btnSetEvent.Font = new System.Drawing.Font("Tahoma", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(162)));
            this.btnSetEvent.Location = new System.Drawing.Point(242, 72);
            this.btnSetEvent.Name = "btnSetEvent";
            this.btnSetEvent.Size = new System.Drawing.Size(207, 177);
            this.btnSetEvent.TabIndex = 1;
            this.btnSetEvent.Text = "Set Event";
            this.btnSetEvent.UseVisualStyleBackColor = true;
            this.btnSetEvent.Click += new System.EventHandler(this.btnSetEvent_Click);
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(483, 327);
            this.Controls.Add(this.btnSetEvent);
            this.Controls.Add(this.btnCreateEvent);
            this.Name = "Form1";
            this.Text = "CreateEvents";
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button btnCreateEvent;
        private System.Windows.Forms.Button btnSetEvent;
    }
}

