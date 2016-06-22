open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.Drawing
open System.Drawing.Design
open System.IO
open System.Linq
open System.Text
open System.Threading
open System.Windows.Forms
open System.Windows.Input
//open System.Windows.Forms.DataVisualization
open System.Windows.Forms.Design
open Microsoft.FSharp.Control

exception Invalid_Simulation_Category

let threadPool = ref ([] : System.Threading.Thread list)

let sema = new Threading.Semaphore(1, 1)

let sharedLineSema = new Threading.Semaphore(1, 1)

let mutable packetCount = 0L

let mutable sharedLineIdle = true

let mutable sharedLine = 0

let mutable nodeCount = 0
let mutable totalExecutionTime = 0
let mutable CW = 0

let ev = new Event<int>()
ev.Publish.Add(fun x ->
    packetCount <- packetCount + 1L
    Thread.Sleep(TimeSpan(System.Convert.ToInt64(x * 8)))
)

let checkIdle (t : int64) =
    let startTimeSpan = System.DateTime.Now.Ticks
    let mutable res = true
    while (System.DateTime.Now.Ticks - startTimeSpan <= t) do
        if not sharedLineIdle then res <- false
    if res then 
        sharedLineSema.WaitOne() |> ignore
        sharedLineIdle <- false
    res

let uniformBackoff (cw : int) =
    (int64)(System.Math.Ceiling(System.Random().NextDouble() * (float)cw))

let binaryBackoff (cw : int64) =
    cw * 2L

let nodeThreadPTSUniformCD = new ParameterizedThreadStart(fun x ->
    let tuple = unbox x
    let nodeNumber = fst tuple
    let timeLimit = snd tuple
    let startTime = System.DateTime.Now.Ticks
    while (System.DateTime.Now.Ticks - startTime <= timeLimit) do
        let mutable semawait = false
        while (semawait = false) do
            semawait <- sharedLineSema.WaitOne(TimeSpan(500L))
            if (semawait = false) then
                Thread.Sleep(TimeSpan(500L * uniformBackoff(CW)))
        ev.Trigger(nodeNumber)
        sharedLineSema.Release() |> ignore
)

let nodeThreadPTSUniform = new ParameterizedThreadStart(fun x ->
    let tuple = unbox x
    let nodeNumber = fst tuple
    let timeLimit = snd tuple
    let startTime = System.DateTime.Now.Ticks
    while (System.DateTime.Now.Ticks - startTime <= timeLimit) do
        let mutable semawait = false
        while (semawait = false) do
            semawait <- sharedLineSema.WaitOne(0)
        ev.Trigger(nodeNumber)
        sharedLineSema.Release() |> ignore
)

let nodeThreadPTSBinary = new ParameterizedThreadStart(fun x ->
    let tuple = unbox x
    let nodeNumber = fst tuple
    let timeLimit = snd tuple
    let startTime = System.DateTime.Now.Ticks
    while (System.DateTime.Now.Ticks - startTime <= timeLimit) do
        let mutable semawait = false
        let mutable cw = 2L
        while (semawait = false) do
            semawait <- sharedLineSema.WaitOne(TimeSpan(500L))
            if (semawait = false) then
                Thread.Sleep(TimeSpan(500L * cw))
                cw <- binaryBackoff(cw)
        ev.Trigger(nodeNumber)
        sharedLineSema.Release() |> ignore
)

let mainThread = new Thread(ThreadStart(fun x ->

    // -----------------------------------------------------------------------------------------------------------
    // Region for GUI Form
    //

    // Main form.
    let mainForm = new Form(Width = 1280, Height = 720, AutoScaleMode = AutoScaleMode.Font, StartPosition = FormStartPosition.CenterScreen, FormBorderStyle = FormBorderStyle.Sizable, Text = "DCSimulation")

    // ----------------------
    // Region for fonts.
    //
    // Fonts.
    let fontMalgunGothic = new Font("맑은 고딕", 10.0f, FontStyle.Regular, GraphicsUnit.Point)
    let fontMalgunGothicLargeBold = new Font("맑은 고딕", 14.0f, FontStyle.Bold, GraphicsUnit.Point)
    let fontMalgunGothicBold = new Font("맑은 고딕", 10.0f, FontStyle.Bold, GraphicsUnit.Point)
    let fontNanumGothic = new Font("나눔고딕", 10.0f, FontStyle.Regular, GraphicsUnit.Point)
    //
    // End region for fonts.
    // ----------------------

    // ----------------------
    // Region for keep-updating labels.
    //
    // Labels - output fields
    let labelOutputCurrentRate = new Label(Text = "N/A", Width = 240, Height = 25, Location = new Point(320, 420), Font = fontMalgunGothic)
    let labelOutputCumulatedRate = new Label(Text = "N/A", Width = 240, Height = 25, Location = new Point(320, 450), Font = fontMalgunGothic)
    let labelOutputResultThroughput = new Label(Text = "N/A", Width = 240, Height = 25, Location = new Point(400, 500), Font = fontMalgunGothic)
    let labelOutputUniformNodeCount = new Label(Text = "N/A", Width = 240, Height = 25, Location = new Point(820, 200), Font = fontMalgunGothic)
    let labelOutputUniformTotalTime = new Label(Text = "N/A", Width = 240, Height = 25, Location = new Point(820, 230), Font = fontMalgunGothic)
    let labelOutputUniformCW = new Label(Text = "N/A", Width = 240, Height = 25, Location = new Point(820, 260), Font = fontMalgunGothic)
    let labelOutputUniformCDNodeCount = new Label(Text = "N/A", Width = 240, Height = 25, Location = new Point(820, 340), Font = fontMalgunGothic)
    let labelOutputUniformCDTotalTime = new Label(Text = "N/A", Width = 240, Height = 25, Location = new Point(820, 370), Font = fontMalgunGothic)
    let labelOutputUniformCDCW = new Label(Text = "N/A", Width = 240, Height = 25, Location = new Point(820, 400), Font = fontMalgunGothic)
    let labelOutputBinaryNodeCount = new Label(Text = "N/A", Width = 240, Height = 25, Location = new Point(820, 480), Font = fontMalgunGothic)
    let labelOutputBinaryTotalTime = new Label(Text = "N/A", Width = 240, Height = 25, Location = new Point(820, 510), Font = fontMalgunGothic)

    // Buttons.
    let buttonNodeCountSet = new Button(Text = "Set", Width = 40, Height = 25, Location = new Point(380, 100), Font = fontMalgunGothic)
    let buttonTotalTimeSet = new Button(Text = "Set", Width = 40, Height = 25, Location = new Point(380, 130), Font = fontMalgunGothic)
    let buttonCWSet = new Button(Text = "Set", Width = 40, Height = 25, Location = new Point(380, 160), Font = fontMalgunGothic)
    let buttonClose = new Button(Text = "Close", Width = 60, Height = 25, Location = new Point(1160, 640), Font = fontMalgunGothic)
    let buttonReset = new Button(Text = "Reset", Width = 60, Height = 25,  Location = new Point(1080, 640), Font = fontMalgunGothic)
    let buttonRunUniformPST = new Button(Text = "Run Uniform Distribution Simulation", Width = 320, Height = 25, Location = new Point(70, 240), Font = fontMalgunGothic)
    let buttonRunUniformPSTCD = new Button(Text = "Run Uniform Distribution /w CD Simulation", Width = 320, Height = 25, Location = new Point(70, 270), Font = fontMalgunGothic)
    let buttonRunBinaryPST = new Button(Text = "Run Binary Distribution /w CD Simulation", Width = 320, Height = 25, Location = new Point(70, 300), Font = fontMalgunGothic)

    // Text input fields.
    let textboxNodeCount = new TextBox(Text = "", Width = 120, Height = 25, Location = new Point(240, 100), Font = fontMalgunGothic)
    let textboxTotalTime = new TextBox(Text = "", Width = 120, Height = 25, Location = new Point(240, 130), Font = fontMalgunGothic)
    let textboxCW = new TextBox(Text = "", Width = 120, Height = 25, Location = new Point(240, 160), Font = fontMalgunGothic)

    // Labels.
    let labelNodeCount = new Label(Text = "Number of Nodes: ", Width = 160, Height = 25, Location = new Point(70, 100), Font = fontMalgunGothic)
    let labelTotalTime = new Label(Text = "Total Execution Time: ", Width = 160, Height = 25, Location = new Point(70, 130), Font = fontMalgunGothic)
    let labelCW = new Label(Text = "CW(Uniform Dist only): ", Width = 160, Height = 25, Location = new Point(70, 160), Font = fontMalgunGothic)
    let labelCurrentRate = new Label(Text = "Current Rate (packets/sec): ", Width = 240, Height = 25, Location = new Point(70, 420), Font = fontMalgunGothic)
    let labelCumulatedRate = new Label(Text = "Cumulated Rate (packets/sec): ", Width = 240, Height = 25, Location = new Point(70, 450), Font = fontMalgunGothic)
    let labelResultThroughput = new Label(Text = "Result - Throughput (packets/sec): ", Width = 320, Height = 25, Location = new Point(70, 500), Font = fontMalgunGothicBold)
    let labelUniformNodeCount = new Label(Text = "Number of Nodes: ", Width = 160, Height = 25, Location = new Point(640, 200), Font = fontMalgunGothic)
    let labelUniformTotalTime = new Label(Text = "Total Execution Time: ", Width = 160, Height = 25, Location = new Point(640, 230), Font = fontMalgunGothic)
    let labelUniformCW = new Label(Text = "CW(Uniform Dist only): ", Width = 160, Height = 25, Location = new Point(640, 260), Font = fontMalgunGothic)
    let labelUniformCDNodeCount = new Label(Text = "Number of Nodes: ", Width = 160, Height = 25, Location = new Point(640, 340), Font = fontMalgunGothic)
    let labelUniformCDTotalTime = new Label(Text = "Total Execution Time: ", Width = 160, Height = 25, Location = new Point(640, 370), Font = fontMalgunGothic)
    let labelUniformCDCW = new Label(Text = "CW(Uniform Dist only): ", Width = 160, Height = 25, Location = new Point(640, 400), Font = fontMalgunGothic)
    let labelBinaryNodeCount = new Label(Text = "Number of Nodes: ", Width = 160, Height = 25, Location = new Point(640, 480), Font = fontMalgunGothic)
    let labelBinaryTotalTime = new Label(Text = "Total Execution Time: ", Width = 160, Height = 25, Location = new Point(640, 510), Font = fontMalgunGothic)

    // Labels - Info fields
    let labelInfoUniform = new Label(Text = "Settings Info - Uniform Distribution", Width = 480, Height = 35, Location = new Point(640, 160), Font = fontMalgunGothicLargeBold)
    let labelInfoUniformCD = new Label(Text = "Settings Info - Uniform Distribution", Width = 480, Height = 35, Location = new Point(640, 300), Font = fontMalgunGothicLargeBold)
    let labelInfoBinary = new Label(Text = "Settings Info - Uniform Distribution", Width = 480, Height = 35, Location = new Point(640, 440), Font = fontMalgunGothicLargeBold)

    // Add control events.
    buttonClose.Click.Add(fun _ ->
        mainForm.Close()
    )

    buttonRunUniformPST.Click.Add(fun _ ->
        threadPool.Value <- [ for i in 1 .. nodeCount -> new Thread(nodeThreadPTSUniform) ]
        let totalTime = (int64)totalExecutionTime * 10000000L
        let startTime = System.DateTime.Now.Ticks
        let mutable lastPacketCount = 0L
        for i=1 to nodeCount do
            threadPool.Value.[i-1].Start((i, totalTime))
        let mutable lastCheckTime = System.DateTime.Now.Ticks
        let checkInterval = 1000000L
        while (System.DateTime.Now.Ticks - startTime <= totalTime) do
            if (System.DateTime.Now.Ticks - lastCheckTime < checkInterval) then
                Thread.Sleep(TimeSpan(100000L))
            else
                let currentPacketCount = packetCount
                printfn "Packet transmission rate is : %d (packets/sec), Cumulated rate is : %f (packets/sec)" ((currentPacketCount - lastPacketCount) * 10L) ((float)currentPacketCount / ((float)(System.DateTime.Now.Ticks - startTime) / 10000000.0))
                labelOutputCurrentRate.Text <- ((currentPacketCount - lastPacketCount) * 10L).ToString()
                labelOutputCumulatedRate.Text <- ((float)currentPacketCount / ((float)(System.DateTime.Now.Ticks - startTime) / 10000000.0)).ToString()
                mainForm.Refresh()
                lastPacketCount <- currentPacketCount
                lastCheckTime <- System.DateTime.Now.Ticks
        for i=1 to nodeCount do
            threadPool.Value.[i-1].Join()
        printfn "Finished calling node Thread. packetCount is : %d" packetCount
        printfn "Total packet transmission rate is : %f (packets/sec)" ((float)packetCount / ((float)totalTime/10000000.0))
        labelOutputResultThroughput.Text <- ((float)packetCount / ((float)totalTime/10000000.0)).ToString()
        mainForm.Refresh()
    )

    buttonRunUniformPSTCD.Click.Add(fun _ ->
        threadPool.Value <- [ for i in 1 .. nodeCount -> new Thread(nodeThreadPTSUniformCD) ]
        let totalTime = (int64)totalExecutionTime * 10000000L
        let startTime = System.DateTime.Now.Ticks
        let mutable lastPacketCount = 0L
        for i=1 to nodeCount do
            threadPool.Value.[i-1].Start((i, totalTime))
        let mutable lastCheckTime = System.DateTime.Now.Ticks
        let checkInterval = 1000000L
        while (System.DateTime.Now.Ticks - startTime <= totalTime) do
            if (System.DateTime.Now.Ticks - lastCheckTime < checkInterval) then
                Thread.Sleep(TimeSpan(100000L))
            else
                let currentPacketCount = packetCount
                printfn "Packet transmission rate is : %d (packets/sec), Cumulated rate is : %f (packets/sec)" ((currentPacketCount - lastPacketCount) * 10L) ((float)currentPacketCount / ((float)(System.DateTime.Now.Ticks - startTime) / 10000000.0))
                labelOutputCurrentRate.Text <- ((currentPacketCount - lastPacketCount) * 10L).ToString()
                labelOutputCumulatedRate.Text <- ((float)currentPacketCount / ((float)(System.DateTime.Now.Ticks - startTime) / 10000000.0)).ToString()
                mainForm.Refresh()
                lastPacketCount <- currentPacketCount
                lastCheckTime <- System.DateTime.Now.Ticks
        for i=1 to nodeCount do
            threadPool.Value.[i-1].Join()
        printfn "Finished calling node Thread. packetCount is : %d" packetCount
        printfn "Total packet transmission rate is : %f (packets/sec)" ((float)packetCount / ((float)totalTime/10000000.0))
        labelOutputResultThroughput.Text <- ((float)packetCount / ((float)totalTime/10000000.0)).ToString()
        mainForm.Refresh()
    )

    buttonRunBinaryPST.Click.Add(fun _ ->
        threadPool.Value <- [ for i in 1 .. nodeCount -> new Thread(nodeThreadPTSBinary) ]
        let totalTime = (int64)totalExecutionTime * 10000000L
        let startTime = System.DateTime.Now.Ticks
        let mutable lastPacketCount = 0L
        for i=1 to nodeCount do
            threadPool.Value.[i-1].Start((i, totalTime))
        let mutable lastCheckTime = System.DateTime.Now.Ticks
        let checkInterval = 1000000L
        while (System.DateTime.Now.Ticks - startTime <= totalTime) do
            if (System.DateTime.Now.Ticks - lastCheckTime < checkInterval) then
                Thread.Sleep(TimeSpan(100000L))
            else
                let currentPacketCount = packetCount
                printfn "Packet transmission rate is : %d (packets/sec), Cumulated rate is : %f (packets/sec)" ((currentPacketCount - lastPacketCount) * 10L) ((float)currentPacketCount / ((float)(System.DateTime.Now.Ticks - startTime) / 10000000.0))
                labelOutputCurrentRate.Text <- ((currentPacketCount - lastPacketCount) * 10L).ToString()
                labelOutputCumulatedRate.Text <- ((float)currentPacketCount / ((float)(System.DateTime.Now.Ticks - startTime) / 10000000.0)).ToString()
                mainForm.Refresh()
                lastPacketCount <- currentPacketCount
                lastCheckTime <- System.DateTime.Now.Ticks
        for i=1 to nodeCount do
            threadPool.Value.[i-1].Join()
        printfn "Finished calling node Thread. packetCount is : %d" packetCount
        printfn "Total packet transmission rate is : %f (packets/sec)" ((float)packetCount / ((float)totalTime/10000000.0))
        labelOutputResultThroughput.Text <- ((float)packetCount / ((float)totalTime/10000000.0)).ToString()
        mainForm.Refresh()
    )

    buttonReset.Click.Add(fun _ ->
        nodeCount <- 0
        totalExecutionTime <- 0
        CW <- 0
        textboxNodeCount.Text <- ""
        textboxTotalTime.Text <- ""
        textboxCW.Text <- ""
        labelOutputUniformNodeCount.Text <- "N/A"
        labelOutputUniformTotalTime.Text <- "N/A"
        labelOutputUniformCW.Text <- "N/A"
        labelOutputUniformCDNodeCount.Text <- "N/A"
        labelOutputUniformCDTotalTime.Text <- "N/A"
        labelOutputUniformCDCW.Text <- "N/A"
        labelOutputBinaryNodeCount.Text <- "N/A"
        labelOutputBinaryTotalTime.Text <- "N/A"
        labelOutputCurrentRate.Text <- "N/A"
        labelOutputCumulatedRate.Text <- "N/A"
        labelOutputResultThroughput.Text <- "N/A"
    )

    buttonNodeCountSet.Click.Add(fun _ ->
        try
            let tmp = System.Convert.ToInt32(textboxNodeCount.Text)
            nodeCount <- tmp
            labelOutputUniformNodeCount.Text <- tmp.ToString()
            labelOutputUniformCDNodeCount.Text <- tmp.ToString()
            labelOutputBinaryNodeCount.Text <- tmp.ToString()
        with
            | e -> textboxNodeCount.Text <- ""
    )

    buttonTotalTimeSet.Click.Add(fun _ ->
        try
            let tmp = System.Convert.ToInt32(textboxTotalTime.Text)
            totalExecutionTime <- tmp
            labelOutputUniformTotalTime.Text <- tmp.ToString()
            labelOutputUniformCDTotalTime.Text <- tmp.ToString()
            labelOutputBinaryTotalTime.Text <- tmp.ToString()
        with
            | e -> textboxTotalTime.Text <- ""
    )

    buttonCWSet.Click.Add(fun _ ->
        try
            let tmp = System.Convert.ToInt32(textboxCW.Text)
            CW <- tmp
            labelOutputUniformCW.Text <- tmp.ToString()
            labelOutputUniformCDCW.Text <- tmp.ToString()
        with
            | e -> textboxCW.Text <- ""
    )

    // Add all controls to form.
    mainForm.Controls.AddRange([| buttonNodeCountSet; buttonTotalTimeSet; buttonCWSet; buttonClose; buttonReset; buttonRunUniformPST; buttonRunUniformPSTCD; buttonRunBinaryPST;
                                  textboxNodeCount; textboxTotalTime; textboxCW;
                                  labelNodeCount; labelTotalTime; labelCW; labelCurrentRate; labelCumulatedRate; labelResultThroughput; labelUniformNodeCount; labelUniformTotalTime; labelUniformCW;
                                  labelUniformCDNodeCount; labelUniformCDTotalTime; labelUniformCDCW; labelBinaryNodeCount; labelBinaryTotalTime;
                                  labelOutputCurrentRate; labelOutputCumulatedRate; labelOutputResultThroughput; labelOutputUniformNodeCount; labelOutputUniformTotalTime; labelOutputUniformCW;
                                  labelOutputUniformCDNodeCount; labelOutputUniformCDTotalTime; labelOutputUniformCDCW; labelOutputBinaryNodeCount; labelOutputBinaryTotalTime;
                                  labelInfoUniform; labelInfoUniformCD; labelInfoBinary |])

    mainForm.ShowDialog() |> ignore

    //
    // End region for keep-updating labels.
    // ---------------------

    //
    // End Region for GUI Form.
    // -----------------------------------------------------------------------------------------------------------

))

let tempMainThread = new Thread(ThreadStart(fun _ ->
    printfn "This is from main thread."
    //let nodeCount = 20
    threadPool.Value <- [ for i in 1 .. nodeCount -> new Thread(nodeThreadPTSUniform) ]
    let totalTime = (int64)totalExecutionTime * 10000000L
    let startTime = System.DateTime.Now.Ticks
    let mutable lastPacketCount = 0L
    for i=1 to nodeCount do
        threadPool.Value.[i-1].Start((i, totalTime))
    let mutable lastCheckTime = System.DateTime.Now.Ticks
    let checkInterval = 1000000L
    while (System.DateTime.Now.Ticks - startTime <= totalTime) do
        if (System.DateTime.Now.Ticks - lastCheckTime < checkInterval) then
            Thread.Sleep(TimeSpan(100000L))
        else
            let currentPacketCount = packetCount
            printfn "Packet transmission rate is : %d (packets/sec), Cumulated rate is : %f (packets/sec)" ((currentPacketCount - lastPacketCount) * 10L) ((float)currentPacketCount / ((float)(System.DateTime.Now.Ticks - startTime) / 10000000.0))
            //labelOutputCurrentRate.Text <- ((currentPacketCount - lastPacketCount) * 10L).ToString()
            //labelOutputCumulatedRate.Text <- ((float)currentPacketCount / ((float)(System.DateTime.Now.Ticks - startTime) / 10000000.0)).ToString()
            //mainForm.Refresh()
            lastPacketCount <- currentPacketCount
            lastCheckTime <- System.DateTime.Now.Ticks
    for i=1 to nodeCount do
        threadPool.Value.[i-1].Join()
    printfn "Finished calling node Thread. packetCount is : %d" packetCount
    printfn "Total packet transmission rate is : %f (packets/sec)" ((float)packetCount / ((float)totalTime/10000000.0))
    //labelOutputResultThroughput.Text <- ((float)packetCount / ((float)totalTime/10000000.0)).ToString()
    //mainForm.Refresh()
))

mainThread.Start()
mainThread.Join()