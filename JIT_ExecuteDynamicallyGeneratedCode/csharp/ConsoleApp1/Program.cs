using System;
using System.IO;
using System.Net;
using System.Runtime.InteropServices;

unsafe class Program
{
    // winnt.h    
    const int MEM_COMMIT = 0x00001000;
    const int MEM_RESERVE = 0x00002000;
    const int MEM_RELEASE = 0x00008000;
    const int PAGE_EXECUTE_READWRITE = 0x40;

    [DllImport("kernel32.dll", SetLastError = true)]
    unsafe static extern IntPtr VirtualAlloc(void* lpAddress, UIntPtr dwSize, int flAllocationType, int flProtect);

    [DllImport("kernel32.dll", SetLastError = true)]
    unsafe static extern bool VirtualFree(IntPtr lpAddress, UIntPtr dwSize, int dwFreeType);


    public delegate int FunctionPtr();
    static byte[] ByteCode = new byte[] { 0xB8, 0x05, 0x00, 0x00, 0x00, 0x83, 0xC0, 0x04, 0xC3 };

    const string FileName = "ByteCode.bin";
    const string ByteCodeURL = "https://github.com/ismailkocacan/Experiment/tree/master/JIT_ExecuteDynamicallyGeneratedCode/tests/" + FileName;

    static void Main(string[] args)
    {
        IntPtr P  = VirtualAlloc(null, (UIntPtr)ByteCode.Length, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
        Marshal.Copy(ByteCode, 0, P, ByteCode.Length);
        FunctionPtr functionPtr = (FunctionPtr)Marshal.GetDelegateForFunctionPointer(P, typeof(FunctionPtr));
        int result = functionPtr();
        VirtualFree(P, (UIntPtr)ByteCode.Length, MEM_RELEASE);
    }

    static byte[] GetByteCode()
    {
        WebClient client = new WebClient();
        byte[] codeData = client.DownloadData(ByteCodeURL);
        return codeData;
    }

    static void SaveToFile()
    {
        File.WriteAllBytes(FileName, ByteCode);
    }
}

