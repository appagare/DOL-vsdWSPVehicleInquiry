Imports System.ServiceProcess
Imports System.Threading
Imports System.Web
Imports WA.DOL.LogEvent.LogEvent

Public Class svrMain
    Inherits System.ServiceProcess.ServiceBase

    'module level parameters
    Private MaxThreads As Integer = 20 'max concurrent threads
    Private ThreadCount As Integer = 0 'number of threads spawned
    Private QueueTxName As String = "PRIVATE$\DBQTx" 'name of the common output queue
    Private QueueRxName As String = "PRIVATE$\Vehicle" 'name of the input queue
    Private QueueServer As String = "DOLUTOLYVSDEV1" 'queue server path name
    Private QueueSleepWhenEmpty As Integer = 500 'number of milliseconds to wait when the queue is empty before checking again
    Private DebugMode As Byte = 0
    Private QueueTxObject As WA.DOL.VSD.WSPQueue.QueueObject 'common output queue object
    Private LogEventObject As New WA.DOL.LogEvent.LogEvent()

    Private ProxyName As String = ""
    Private UserName As String = ""
    Private Password As String = ""
    Private Domain As String = ""
    Private UseSystemCredentials As Boolean = False
    Private PreAuthenticate As Boolean = False
    Private Credentials As Net.NetworkCredential
    Private Proxy As New System.Net.WebProxy()


    Private Enum ServiceStates
        Shutdown = 0
        Paused = 1
        Running = 2
    End Enum

    Private ServiceState As ServiceStates = ServiceStates.Paused

#Region " Component Designer generated code "

    Public Sub New()
        MyBase.New()

        ' This call is required by the Component Designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call

    End Sub

    'UserService overrides dispose to clean up the component list.
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub

    ' The main entry point for the process
    <MTAThread()> _
    Shared Sub Main()
        Dim ServicesToRun() As System.ServiceProcess.ServiceBase

        ' More than one NT Service may run within the same process. To add
        ' another service to this process, change the following line to
        ' create a second service object. For example,
        '
        '   ServicesToRun = New System.ServiceProcess.ServiceBase () {New Service1, New MySecondUserService}
        '
        ServicesToRun = New System.ServiceProcess.ServiceBase() {New svrMain()}

        System.ServiceProcess.ServiceBase.Run(ServicesToRun)
    End Sub

    'Required by the Component Designer
    Private components As System.ComponentModel.IContainer

    ' NOTE: The following procedure is required by the Component Designer
    ' It can be modified using the Component Designer.  
    ' Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        '
        'svrMain
        '
        Me.CanShutdown = True
        Me.ServiceName = "WSP Vehicle Inquiry"

    End Sub

#End Region

    Protected Overrides Sub OnStart(ByVal args() As String)
        ' Add code here to start your service. This method should set things
        ' in motion so your service can do its work.

        Try
            'read the app settings
            ReadAppSettings()

            'validate settings
            If Not ValidSettings() Then
                'we read all of the values but do not 
                'have valid parameters and thus are unable 
                'to continue. Throw error to drop into exception catch
                '
                Throw New Exception("Invalid settings at start up. Unable to proceed.")
            End If

            'set the credentials if present
            If UserName <> "" AndAlso Password <> "" AndAlso Domain <> "" Then
                'use the credentials supplied in the .cong file
                Credentials = New Net.NetworkCredential(UserName, Password, Domain)
            ElseIf UseSystemCredentials = True Then
                'use the credentials of the account we are running under
                Credentials = System.Net.CredentialCache.DefaultCredentials()
            End If

            'set the proxy if a name is specified
            If ProxyName <> "" Then
                Proxy = New System.Net.WebProxy(ProxyName, True)
                'set the credentials of the proxy if we have credentials
                If Not Credentials Is Nothing Then
                    Proxy.Credentials = Credentials
                End If
            End If

            'instantiate our common output queue object
            QueueTxObject = New WA.DOL.VSD.WSPQueue.QueueObject(QueueServer, QueueTxName)


        Catch ex As Exception

            'LogEvent, Send E-mail, and quit
            Dim strMessage As String = "Service is unable to proceed. Shutting down. " & ex.Message
            'log the error
            LogEvent("Service_OnStart", strMessage, MessageType.Error, LogType.Standard)

            OnStop()
            Exit Sub

        End Try

        'set our status to run mode
        ServiceState = ServiceStates.Running

        'make note that we started
        LogEvent("OnStart", "Service Started", MessageType.Start, LogType.Standard)

        'start an endless loop for processing the queue
        ThreadPool.QueueUserWorkItem(AddressOf ProcessQueue)

    End Sub

    Protected Overrides Sub OnStop()
        ' Add code here to perform any tear-down necessary to stop your service.

        'warn threads we are shutting down
        ServiceState = ServiceStates.Shutdown

        'give threads 20 seconds to wrap things up (this should be more than enough time)
        Dim EndWait As Date = Now.AddSeconds(20)
        While Now <= EndWait
            If ThreadCount = 0 Then
                Exit While
            End If
        End While

        'log event that we are stopping
        LogEvent("OnStop", "Service Stopped", MessageType.Finish, LogType.Standard)

    End Sub

    Private Function FixNLETs(ByVal Response As String) As String
        'This should ONLY be called when the IsNLETs flag is True!

        Select Case Left(Response, 3)
            Case "RQ.", "V2.", "V3.", "V4.", "V5.", "L .", "V .", "T .", "RR.", "RQG", "RRG", "SQ.", "SR."
                'nothing preppended, return what was received
                Return Response
            Case Else
                'see if its a 2 character response
                Select Case Left(Response, 2)
                    Case "L.", "V.", "T."
                        'nothing preppended, return what was received
                        Return Response
                    Case Else
                        'strip region field

                        'L1RA    .RR..AZNLETS22.LIC/003LLR
                        'L1RA    .RR.IDNLETS10.*SLW1      .LIC/003LLR

                        'find the first "." and remove everything to the left of it, including the period
                        Dim Position As Integer = InStr(Response, ".")
                        If Position > 1 Then
                            '
                            Response = Right(Response, Len(Response) - Position)
                            If Left(Response, 4) <> "RR.." Then
                                'strip the first RR. off and replace it with a RR..
                                Return Replace(Response, "RR.", "RR..", 1, 1)
                            Else
                                Return Response
                            End If
                        Else
                            'shouldn't happen, but return response
                            Return Response
                        End If
                End Select
        End Select

    End Function

    Private Function FixRegional(ByVal Response As String) As String
        'This should ONLY be called when the IsNLETs flag is False!

        Select Case Left(Response, 3)
            Case "RQ.", "V2.", "V3.", "V4.", "V5.", "L .", "V .", "T .", "RR.", "RQG", "RRG", "SQ.", "SR."
                'nothing preppended, return what was received
                Return Response
            Case Else
                'see if its a 2 character response
                Select Case Left(Response, 2)
                    Case "L.", "V.", "T."
                        'nothing preppended, return what was received
                        Return Response
                    Case Else
                        'strip region field

                        'L1RA    .RR..AZNLETS22.LIC/003LLR
                        'L1RA    .RR.IDNLETS10.*SLW1      .LIC/003LLR

                        'find the first "." and remove everything to the left of it, including the period
                        Dim Position As Integer = InStr(Response, ".")
                        If Position > 1 Then
                            Return Right(Response, Len(Response) - Position)
                        Else
                            'shouldn't happen, but return response
                            Return Response
                        End If
                End Select
        End Select

    End Function

    Private Sub LogEvent(ByVal Source As String, _
        ByVal Message As String, ByVal MessageType As MessageType, _
        ByVal LogType As LogType)
        'Purpose:   Write an event to the event log database of the specified type.
        'Input:     Source = source procedure reporting the event.
        '           Message = event message.
        '           MessageType = LogEvent object indicator specifying whether the 
        '           message is error, informational, start, finish, or debug
        '           LogType = LogEvent object indicator
        'Returns:   None
        'Note:      When an LogType is error, an e-mail is automatically sent

        'log message
        LogEventObject.LogEvent(Me.ServiceName, Source, Message, MessageType, LogType)

        'if message type is an error, also log an email event
        If MessageType = MessageType.Error Then
            LogEventObject.LogEvent(Me.ServiceName, Source, Message, MessageType, LogType.Email)
        End If

    End Sub

    Private Sub ProcessMessage(ByVal State As Object)
        'Purpose:   Worker thread process to run the Fujitsu program
        'Input:     State - new thread callback interface containing 
        '           a WSPMessage object.
        'Returns:   None.

        'Dim DEBUG_STRING As String = "1. Start Procedure: " & FormatDateTime(Now, DateFormat.LongTime) & "|"

        Dim WspRequest As New WA.DOL.VSD.WSPQueue.WSPMessage()
        Dim WspResponse As New WA.DOL.VSD.WSPQueue.WSPMessage()
        Dim datStart As Date = Now
        'get the object into a typed class
        WspRequest = CType(State, WA.DOL.VSD.WSPQueue.WSPMessage)

        'log the request
        If DebugMode > 0 Then
            LogEvent("ProcessMessage", "Request: " & WspRequest.Body, MessageType.Debug, LogType.Standard)
        End If

        'pass message to Fujitsu EXE
        Const strUNAVAILABLE As String = "TEMPORARILY UNAVAILABLE "
        Dim strResponse As String = strUNAVAILABLE
        Dim MAXSM As New dolservicepredev.dolWSPservice()

        Try

            'if we have credentials, assign them to the web service request
            If Not Credentials Is Nothing Then
                MAXSM.Credentials = Credentials
            End If

            'if we have a proxy, assign it to the web service request
            If Not Proxy Is Nothing Then
                MAXSM.Proxy = Proxy
            End If

            'set PreAuthenticate
            MAXSM.PreAuthenticate = PreAuthenticate

            Dim IsNLETs As Boolean = False
            Dim IsRQG As Boolean = False
            'make the call
            Select Case Left(WspRequest.Body, 3)
                Case "RQ.", "RQG", "SQ."
                    'NLETS
                    IsNLETs = True
                    If Left(WspRequest.Body, 4) = "RQG." Then
                        'regional NLETS - inquire as RQ.
                        IsRQG = True
                        strResponse = MAXSM.CallMAXSM("RARR." & Replace(WspRequest.Body, "RQG.", "RQ.", 1, 1))
                    Else
                        'normal NLETS
                        strResponse = MAXSM.CallMAXSM("RARR." & WspRequest.Body)
                    End If
                Case Else
                    'in-state
                    strResponse = MAXSM.CallMAXSM(IIf(Left(WspRequest.Body, 5) <> "RARR.", "RARR.", "") & WspRequest.Body)
            End Select

            'expand LFs to CRLFs
            strResponse = Replace(Replace(strResponse, vbCr, ""), vbLf, vbCrLf)

            If IsNLETs = True Then
                'fix the NLETS response
                strResponse = FixNLETs(strResponse)

                If IsRQG = True Then
                    'convert NLETS RR. response back to RRG. response if inquiry was an RQG
                    strResponse = Replace(strResponse, "RR.", "RRG.", 1, 1)
                End If

                IsRQG = False
                IsNLETs = False
            Else
                'added 7-26-2004 per WSP
                'fix the regional responses
                strResponse = FixRegional(strResponse)
            End If

        Catch ex As Exception
            'if this fails, log event and send e-mail
            'no response message will be sent
            Dim strMessage As String = "Error from MAXSM" & vbCrLf & "Request: " & WspRequest.Body & vbCrLf & vbCrLf & "Error: " & ex.Message

            'log the error
            LogEvent("ProcessMessage", strMessage, MessageType.Error, LogType.Standard)
        End Try

        If Trim(strResponse) = "" Then
            'handle null or SQL not available situation from Fujitsu 
            LogEvent("ProcessMessage", "Empty response from MAXSM", MessageType.Error, LogType.Standard)
            strResponse = strUNAVAILABLE
        ElseIf Left(strResponse, 2) = "s!" Or Left(strResponse, 2) = "!!" Then
            'handle errors from MAXSM ("s!..." or "!!...")
            LogEvent("ProcessMessage", strResponse, MessageType.Error, LogType.Standard)
            strResponse = strUNAVAILABLE
        End If

        'assign the response to the queue body
        WspResponse.Body = strResponse

        WspResponse.MessageDate = Now 'set the current time
        WspResponse.Auxiliary = WspRequest.Auxiliary 'pass the Aux through
        WspResponse.Delimiter = WspRequest.Delimiter 'pass the delimiter through
        WspResponse.QueueName = QueueTxName 'specify the output queue name (not required)
        WspResponse.OriginatingID = "" 'the vsMSSGatewayDB will fill this in
        WspResponse.Mnemonic = WspRequest.OriginatingID 'set the destination mnemonic to the originating ID

        'log the response
        If DebugMode > 0 Then
            Dim RunLength As System.TimeSpan = Now.Subtract(datStart)
            Dim Millisecs As Integer = RunLength.TotalMilliseconds
            LogEvent("ProcessMessage", "Response: (" & Millisecs.ToString & " ms) " & WspResponse.Body, MessageType.Debug, LogType.Standard)
        End If

        'lock the common send object and send the response
        SyncLock QueueTxObject
            QueueTxObject.SendMessage(WspResponse)
        End SyncLock
        'clean up
        WspRequest = Nothing
        WspResponse = Nothing
        MAXSM = Nothing

        'decrement the thread count
        Interlocked.Decrement(ThreadCount)

    End Sub

    Private Sub ProcessQueue(ByVal State As Object)
        'Purpose: Main thread to monitor the queue for inquiries.

        'create a Rx object to read the queue
        Dim QueueRxObject As New WA.DOL.VSD.WSPQueue.QueueObject(QueueServer, QueueRxName)

        'loop here while service is running
        While ServiceState = ServiceStates.Running

            Dim intAvailableThreads As Integer = 0
            Dim intIOThreads As Integer = 0

            'check resource availability
            ThreadPool.GetMaxThreads(intAvailableThreads, intIOThreads)

            Try
                '
                'if there is at least one message in the queue and we have resources, 
                'start reading the queue
                If QueueRxObject.CanRead = True AndAlso ThreadCount < intAvailableThreads AndAlso ThreadCount <= MaxThreads Then
                    '
                    'read all of the messages in the queue or until we've been shutdown
                    Do While QueueRxObject.CanRead AndAlso ServiceState = ServiceStates.Running

                        'make sure we still have resource availability
                        ThreadPool.GetMaxThreads(intAvailableThreads, intIOThreads)

                        If ThreadCount >= intAvailableThreads Or ThreadCount >= MaxThreads Then
                            'bail out when no threads are available or max hit;
                            'this will ultimately drop us into Sleep mode
                            Exit Do
                        End If

                        'fetch the queue message
                        Dim QueueMessage As New WA.DOL.VSD.WSPQueue.WSPMessage()
                        QueueMessage = QueueRxObject.ReadMessage
                        QueueMessage.Body = QueueMessage.Body & " "

                        'increment the thread count (each thread will decrement this when its done)
                        Interlocked.Increment(ThreadCount)

                        'process each message on a separate thread
                        ThreadPool.QueueUserWorkItem(AddressOf ProcessMessage, QueueMessage)
                    Loop
                Else
                    '
                    'queue is empty or currently at the max thread count, so take nap
                    Thread.Sleep(QueueSleepWhenEmpty)
                End If
            Catch ex As Exception
                'log the response
                If DebugMode > 0 Then
                    Dim strMessage As String = ex.Message & vbCrLf & vbCrLf & "Available Threads: " & CStr(intAvailableThreads) & vbCrLf & "Thread Count=" & CStr(ThreadCount)
                    LogEvent("_ProcessQueue", "Err: " & strMessage, MessageType.Debug, LogType.Standard)
                End If
                Thread.Sleep(QueueSleepWhenEmpty * 10)
            End Try


        End While 'main loop

    End Sub

    Private Function ReadAppSetting(ByVal Key As String) As String
        'Purpose:   Retrieve parameter from app.config.
        'Input:     Key whose value is being sought.
        'Return:    String value of key.

        On Error Resume Next
        Dim AppSettingsReader As New System.Configuration.AppSettingsReader()
        Dim strReturnValue As String = ""
        Key = Trim(Key)
        If Key <> "" Then
            'get the value
            strReturnValue = AppSettingsReader.GetValue(Key, strReturnValue.GetType())
        End If
        AppSettingsReader = Nothing
        Return strReturnValue
    End Function

    Private Sub ReadAppSettings()
        'Purpose:   Read the necessary application settings

        On Error Resume Next

        If ReadAppSetting("QueueTx") <> "" Then
            QueueTxName = ReadAppSetting("QueueTx")
        End If

        If ReadAppSetting("QueueRx") <> "" Then
            QueueRxName = ReadAppSetting("QueueRx")
        End If

        If ReadAppSetting("QueueServer") <> "" Then
            QueueServer = ReadAppSetting("QueueServer")
        End If

        If IsNumeric(ReadAppSetting("QueueSleepWhenEmpty")) AndAlso _
            CType(ReadAppSetting("QueueSleepWhenEmpty"), Integer) > 0 Then
            QueueSleepWhenEmpty = CType(ReadAppSetting("QueueSleepWhenEmpty"), Integer)
        End If

        If IsNumeric(ReadAppSetting("DebugMode")) AndAlso _
            CType(ReadAppSetting("DebugMode"), Byte) >= 0 Then
            DebugMode = CType(ReadAppSetting("DebugMode"), Byte)
        End If

        If IsNumeric(ReadAppSetting("MaxConcurrentThreads")) AndAlso _
            CType(ReadAppSetting("MaxConcurrentThreads"), Integer) > 0 Then
            MaxThreads = CType(ReadAppSetting("MaxConcurrentThreads"), Integer)
        End If

        If ReadAppSetting("Proxy") <> "" Then
            ProxyName = ReadAppSetting("Proxy")
        End If

        If ReadAppSetting("User") <> "" Then
            UserName = ReadAppSetting("User")
        End If

        If ReadAppSetting("Password") <> "" Then
            Password = ReadAppSetting("Password")
        End If

        If ReadAppSetting("Domain") <> "" Then
            Domain = ReadAppSetting("Domain")
        End If

        If LCase(ReadAppSetting("UseSystemCredentials")) = "true" Then
            UseSystemCredentials = True
        End If

        If LCase(ReadAppSetting("PreAuthenticate")) = "true" Then
            PreAuthenticate = True
        End If

    End Sub

    Private Sub ReturnMessage(ByVal QueueMessage As WA.DOL.VSD.WSPQueue.WSPMessage)
        'Purpose:   used if we need to return a message to the queue
        Dim QueueRxObject As New WA.DOL.VSD.WSPQueue.QueueObject(QueueServer, QueueRxName)
        QueueRxObject.SendMessage(QueueMessage)
        QueueRxObject = Nothing
    End Sub

    Private Function ValidSettings() As Boolean
        'Purpose:   Verify we have valid settings
        If QueueTxName = "" Or QueueRxName = "" Or QueueServer = "" Then
            Return False
        End If
        Return True
    End Function

    Protected Overrides Sub OnShutdown()
        'calls the Windows service OnStop method
        OnStop()
    End Sub

End Class
