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
open System.Windows.Forms.DataVisualization
open System.Windows.Forms.Design
open Microsoft.FSharp.Control

(*
-------------------------------
This part is for simulation constants.
-------------------------------
=======================================================================================
-------------------------------
Constants
-------------------------------
1 unit time = 100ns = time to transmit 1 bit
Slot time = 50us = 500 unit time
1 packet transmission time = 8192 unit time
Collision detection period = 50us = 500 unit time
Max packet generation time (E[T]) = 10ms = 100000 unit time
Initial CW for binary exponential = 2
-------------------------------
=======================================================================================
-------------------------------
End of simulation constants.
-------------------------------
*)

(*
--------------------------------
This part is for CSMA, CSMA/CD, 1-persistent, binary-exponential backoff descriptions.
--------------------------------
=======================================================================================
--------------------------------
CSMA + 1-persistent + Uniform Backoff
--------------------------------
CSMA is abbreiviation for 'Carrier Sense Multiple Access'.
For our simulation, it acts as follows.
1) If medium idle, transmit.
2) After transmit, Wait for 1 unit time (100 ns) to receive ACK(assume no size).
3) If no ACK is provided, think as collision, wait for backoff then step 1.
4) If medium busy, wait for medium to be idle and transmit immediately.
--------------------------------
=======================================================================================
--------------------------------
CSMA/CD + 1-persistent + Uniform Backoff
--------------------------------
CSMA/CD is abbreviation for 'Carrier Sense Multiple Access with Collision Detection'.
For our simulation, it acts as follows.
1) If medium idle, if start of slot, monitor the medium for 50us (collision detection period) to check if idle.
2) If still idle for 50us, transmit.
3) If not idle before 50us, think as collision (all station cease transmission, for collided stations, backoff), then step 1.
4) If medium busy, wait for medium to be idle and repeat from step 1.
--------------------------------
=======================================================================================
--------------------------------
CSMA/CD + 1-persistent + Binary-Exponential Backoff
--------------------------------
CSMA/CD is abbreviation for 'Carrier Sense Multiple Access with Collision Detection'.
For our simulation, it acts as follows.
1) If medium idle, if start of slot, monitor the medium for 50us (collision detection period) to check if idle.
2) If still idle for 50us, transmit.
3) If not idle before 50us, think as collision (all station cease transmission, for collided stations, backoff), then step 1.
4) If medium busy, wait for medium to be idle and repeat from step 1.
--------------------------------
=======================================================================================
--------------------------------
End of description.
--------------------------------

*)

type station (name : string, chargeTime : int) =
    member val name = name
    member val backoff = -1 with get, set
    member val chargeTime = chargeTime with get, set
    member val transmit = false with get, set
    member val transmitTime = -1 with get, set
    member val currentPacketNumber = 0 with get, set
    member val waitACK = -1 with get, set
    member val ackTransmit = false with get, set
    member val ackTransmitTime = -1 with get, set
    member val binaryCW = 2 with get, set
    member val packetTrial = 0 with get, set
    member val collision = 0 with get, set
    member val packetStart = 0L with get, set
    member val packetEnd = 0L with get, set

    member st.GeneratePacket () =
        let rnd = System.Random()
        st.chargeTime <- 0 - (int)(100000.0 * Math.Log(rnd.NextDouble()))
        st.currentPacketNumber <- st.currentPacketNumber + 1

    member st.IsPacketReady () =
        st.chargeTime = 0

    member st.ChargePacket () =
        st.chargeTime <- st.chargeTime - 1

    member st.ConsumeBackoff () =
        st.backoff <- st.backoff - 1

    member st.updateBinaryCW () =
        if st.binaryCW < 1024 then
            st.binaryCW <- st.binaryCW * 2
    

let mutable timeTick = 0L 

let mutable nodeCount = 0
let mutable totalExecutionTime = 0 // (Unit : second)
let mutable CW = 0

let mutable packetCount = 0L
let mutable collisionCount = 0
let mutable packetTrialCount = 0L
let mutable perStationCollisionCount = 0
let mutable packetDelay = 0I

let mutable sharedLine = false
let mutable lineOccupier = 0

let mutable resultThroughput = 0.0
let mutable resultMeanPacketDelay = 0I
let mutable resultCollisionProbability = 0.0

let fst (a, _, _, _) = a
let snd (_, a, _, _) = a
let thd (_, _, a, _) = a
let fourth (_, _, _, a) = a

let stationPool = ref ([] : station list)

let packetPool = ref ([] : (string * int * bool ref * bool) list) // Station name * packet number * isCollided * isACKframe

let uniformBackoff (cw : int) =
    (int32)(System.Math.Ceiling(System.Random().NextDouble() * (float)cw))

let IsMediumIdle () =
    packetPool.Value.Count() <= 0
    
let mutable idleTicks = 0L
let mutable busyTicks = 0L

let CheckCollision () =
    let packetPoolCount = packetPool.Value.Count()
    if packetPoolCount > 1 then
        //printfn "!!!Collision!!!"
        for i = 0 to packetPoolCount - 1 do
            let packetItem = packetPool.Value.Item(i)
            (thd packetItem).Value <- true
    //let mutable res = false
    if packetPoolCount > 0 then busyTicks <- busyTicks + 1L else idleTicks <- idleTicks + 1L
    //for pkt in packetPool.Value do
        //res <- res || (thd pkt).Value
    //res
    packetPoolCount > 1
    
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
let buttonAutoTest = new Button(Text = "Auto Test", Width = 120, Height = 25, Location = new Point(950, 640), Font = fontMalgunGothic)

// Text input fields.
let textboxNodeCount = new TextBox(Text = "", Width = 120, Height = 25, Location = new Point(240, 100), Font = fontMalgunGothic)
let textboxTotalTime = new TextBox(Text = "", Width = 120, Height = 25, Location = new Point(240, 130), Font = fontMalgunGothic)
let textboxCW = new TextBox(Text = "", Width = 120, Height = 25, Location = new Point(240, 160), Font = fontMalgunGothic)
let textboxAutoTestNodeCounts = new TextBox(Text = "", Width = 80, Height = 25, Location = new Point(180, 640), Font = fontMalgunGothic)
let textboxAutoTestExecutionTimes = new TextBox(Text = "", Width = 80, Height = 25, Location = new Point(490, 640), Font = fontMalgunGothic)
let textboxAutoTestCWs = new TextBox(Text = "", Width = 80, Height = 25, Location = new Point(770, 640), Font = fontMalgunGothic)

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
let labelAutoTestNodeCounts = new Label(Text = "Node Count: ", Width = 100, Height = 25, Location = new Point(70, 640), Font = fontMalgunGothic)
let labelAutoTestExecutionTimes = new Label(Text = "Execution Time: ", Width = 120, Height = 25, Location = new Point(350, 640), Font = fontMalgunGothic)
let labelAutoTestCWs = new Label(Text = "CW: ", Width = 120, Height = 25, Location = new Point(640, 640), Font = fontMalgunGothic)
let labelAutoTestUniformPST = new Label(Text = "CSMA-Uniform", Width = 100, Height = 25, Location = new Point(70, 600), Font = fontMalgunGothic)
let labelAutoTestUniformPSTCD = new Label(Text = "CSMA/CD-Uniform", Width = 130, Height = 25, Location = new Point(350, 600), Font = fontMalgunGothic)
let labelAutoTestBinaryPST = new Label(Text = "CSMA/CD-Binary", Width = 120, Height = 25, Location = new Point(640, 600), Font = fontMalgunGothic)

// Checkboxes - Auto test schemes.
let checkboxAutoTestUniformPST = new CheckBox(Location = new Point(180, 600))
let checkboxAutoTestUniformPSTCD = new CheckBox(Location = new Point(490, 600))
let checkboxAutoTestBinaryPST = new CheckBox(Location = new Point(770, 600))

// Labels - Info fields
let labelInfoUniform = new Label(Text = "Settings Info - Uniform Distribution", Width = 480, Height = 35, Location = new Point(640, 160), Font = fontMalgunGothicLargeBold)
let labelInfoUniformCD = new Label(Text = "Settings Info - Uniform Distribution", Width = 480, Height = 35, Location = new Point(640, 300), Font = fontMalgunGothicLargeBold)
let labelInfoBinary = new Label(Text = "Settings Info - Uniform Distribution", Width = 480, Height = 35, Location = new Point(640, 440), Font = fontMalgunGothicLargeBold)

// Add control events.
buttonClose.Click.Add(fun _ ->
    mainForm.Close()
)

buttonRunUniformPSTCD.Click.Add(fun _ ->
    printfn "===================="
    printfn "Simulation Info: "
    printfn "    Scheme:         1-persistent CSMA/CD + Uniform-distribution backoff"
    printfn "    Node Count:     %d" nodeCount
    printfn "    CW:             %d" CW
    printfn "    Execution Time: %d (sec)" totalExecutionTime
    printfn "===================="
    printfn "Now starting simulation...\n"

    let rnd = System.Random()
    stationPool.Value <- [ for i in 1 .. nodeCount -> new station(i.ToString(), (0 - (int)(100000.0 * Math.Log(rnd.NextDouble())))) ]
    //for st in stationPool.Value do
        //st.chargeTime <- 500 - st.chargeTime % 500 + st.chargeTime // Set time to slot.
    let totalTime = (int64)totalExecutionTime * 10000000L // Total time to execute simulation. Unit : 1 unit time (100 ns).
    let startTime = 0L
    let mutable lastPacketCount = 0L
    let mutable lastCheckTime = 0L
    while (timeTick < totalTime) do
        let isIdleTick = IsMediumIdle()
        for st in stationPool.Value do
            if st.transmit && st.transmitTime > 0  then // When transmitting.
                st.transmitTime <- st.transmitTime - 1
                let isCollision = CheckCollision()
                if isCollision then
                    for pkt in packetPool.Value do
                        let pktStation = stationPool.Value.Single(fun x -> x.name = (fst pkt))
                        pktStation.backoff <- 500 * (rnd.Next(1, CW + 1))
                        pktStation.transmitTime <- -1
                        pktStation.transmit <- false
                        pktStation.collision <- pktStation.collision + 1
                    collisionCount <- collisionCount + 1
                    packetPool.Value <- ([] : (string * int * bool ref * bool) list)
            else if st.transmit && st.transmitTime = 0 then // When transmission finished.
                let pkt = packetPool.Value.Single(fun x -> ((fst x) = st.name) && ((snd x) = st.currentPacketNumber) && ((fourth x) = false))
                //printfn "Successful packet transmission from station %s, packet number %d, time tick duration: [%d, %d]" (fst pkt) (snd pkt) (timeTick - 8192L) timeTick
                packetCount <- packetCount + 1L
                st.transmit <- false
                st.transmitTime <- -1
                sharedLine <- false
                st.packetEnd <- timeTick
                packetDelay <- packetDelay + Numerics.BigInteger(st.packetEnd - st.packetStart)
                st.GeneratePacket()
                packetPool.Value <- List.ofSeq(packetPool.Value.Except([pkt]))
            else // When not transmitting.
                if st.backoff > 0 then // When waiting for backoff.
                    st.ConsumeBackoff()
                else if st.backoff = 0 then // When backoff finished.
                    //if IsMediumIdle() then // When medium is idle, transmit.
                    if isIdleTick then
                        st.backoff <- -1
                        st.transmit <- true
                        st.transmitTime <- 8192
                        sharedLine <- true
                        st.packetTrial <- st.packetTrial + 1
                        packetPool.Value <- (st.name, st.currentPacketNumber, ref false, false) :: packetPool.Value
                else if st.IsPacketReady() then // When packet is ready and no backoff.
                    //if IsMediumIdle() then // When medium is idle, transmit.
                    if isIdleTick then
                        st.transmit <- true
                        st.transmitTime <- 8192
                        sharedLine <- true
                        st.packetTrial <- st.packetTrial + 1
                        st.packetStart <- timeTick
                        packetPool.Value <- (st.name, st.currentPacketNumber, ref false, false) :: packetPool.Value
                else // When packet is not ready.
                    st.ChargePacket()

        if (timeTick - lastCheckTime > 10000000L) then
            printfn "Current packetCount: %d, Current throughput: %f (packets/sec), Cumulated throughput: %f (packets/sec)" packetCount ((float)(packetCount - lastPacketCount) / ((float)(timeTick - lastCheckTime) / 10000000.0)) ((float)packetCount / ((float)timeTick / 10000000.0))
            //printfn "Current packetCount: %d" packetCount
            lastCheckTime <- timeTick
            lastPacketCount <- packetCount

        timeTick <- timeTick + 1L // Next simulated-time. (Equal to next unit time.)
    labelOutputResultThroughput.Text <- ((float)packetCount / ((float)(totalTime) / 10000000.0)).ToString()
    mainForm.Refresh()
    for st in stationPool.Value do
        packetTrialCount <- packetTrialCount + (int64)st.packetTrial
        perStationCollisionCount <- perStationCollisionCount + st.collision
    printfn "===================="
    printfn "End of simulation"
    printfn "Total packets transmitted: %d" packetCount
    printfn "Throughput: %f" ((float)packetCount / ((float)(totalTime) / 10000000.0))
    printfn "Total collisions: %d" collisionCount
    printfn "Total per-station collisions: %d" perStationCollisionCount
    printfn "Total transmission attempt: %d" packetTrialCount
    if packetCount = 0L then
        printfn "Mean packet dealy: 0"
    else
        printfn "Mean packet delay: %A" (packetDelay / Numerics.BigInteger(packetCount))
    printfn "Total Ticks: %d" timeTick
    printfn "Total idle ticks: %d" idleTicks
    printfn "Total busy ticks: %d" busyTicks
    printfn "===================="
    resultThroughput <- ((float)packetCount / ((float)(totalTime) / 10000000.0))
    if packetCount = 0L then
        resultMeanPacketDelay <- -1I
    else
        resultMeanPacketDelay <- (packetDelay / Numerics.BigInteger(packetCount))
    resultCollisionProbability <- (float)perStationCollisionCount / (float)packetTrialCount
    printfn "===================="
    printfn "Simulation Result: "
    printfn "    Throughput:                         %f" resultThroughput
    printfn "    Mean Packet Delay:                  %A" resultMeanPacketDelay
    printfn "    Transmission Collision Probability: %f" resultCollisionProbability
    printfn "===================="
)

buttonRunUniformPST.Click.Add(fun _ ->
    printfn "===================="
    printfn "Simulation Info: "
    printfn "    Scheme:         1-persistent CSMA + Uniform-distribution backoff"
    printfn "    Node Count:     %d" nodeCount
    printfn "    CW:             %d" CW
    printfn "    Execution Time: %d (sec)" totalExecutionTime
    printfn "===================="
    printfn "Now starting simulation...\n"

    let rnd = System.Random()
    stationPool.Value <- [ for i in 1 .. nodeCount -> new station(i.ToString(), (0 - (int)(100000.0 * Math.Log(rnd.NextDouble())))) ]
    //stationPool.Value <- [ for i in 1 .. nodeCount -> new station(i.ToString(), 1)]
    let totalTime = (int64)totalExecutionTime * 10000000L // Total time to execute simulation. Unit : 1 unit time (100 ns).
    let startTime = 0L
    let mutable lastPacketCount = 0L
    let mutable lastCheckTime = 0L
    while (timeTick < totalTime) do
        let isIdleTick = IsMediumIdle()
        for st in stationPool.Value do
            if st.waitACK > 0 then // ACK wait time.
                if not st.ackTransmit && isIdleTick then // If ACK is not transmitted
                    sharedLine <- true
                    packetPool.Value <- (st.name, st.currentPacketNumber, (ref false), true) :: packetPool.Value
                    st.ackTransmitTime <- 8193
                    st.ackTransmit <- true
                if st.ackTransmitTime > 0 then
                    st.ackTransmitTime <- st.ackTransmitTime - 1
                st.waitACK <- st.waitACK - 1
            else if st.waitACK = 0 then // ACK wait time finished.
                if st.ackTransmit then // When ACK frame started tranmission.
                    let ackPkt = packetPool.Value.Single(fun x -> ((fst x) = st.name) && ((snd x) = st.currentPacketNumber) && ((fourth x) = true))
                    if (thd ackPkt).Value || st.ackTransmitTime > 0 then // When collision occured of ACK packet or ACK frame does not arrvied yet.
                        st.backoff <- 500 * (rnd.Next(1, CW + 1))
                        collisionCount <- collisionCount + 1
                        st.collision <- st.collision + 1
                        st.waitACK <- -1
                        st.ackTransmitTime <- -1
                        st.ackTransmit <- false
                        sharedLine <- false
                    else // When no collision for ACK packet.
                        //printfn "Successful received ACK for station %s, packet number %d, time tick duration: [%d, %d]" (fst ackPkt) (snd ackPkt) (timeTick - 8193L) timeTick
                        packetCount <- packetCount + 1L
                        sharedLine <- false
                        st.waitACK <- -1
                        st.ackTransmitTime <- -1
                        st.ackTransmit <- false
                        st.packetEnd <- timeTick
                        packetDelay <- packetDelay + Numerics.BigInteger(st.packetEnd - st.packetStart)
                        st.GeneratePacket()
                    packetPool.Value <- List.ofSeq(packetPool.Value.Except([ackPkt]))
                else // When ACK frame does not started transmission.
                    st.backoff <- 500 * (rnd.Next(1, CW + 1))
                    collisionCount <- collisionCount + 1
                    st.collision <- st.collision + 1
                    st.waitACK <- -1
                    st.ackTransmitTime <- -1
                    st.ackTransmit <- false
                    sharedLine <- false
            else if st.transmit && st.transmitTime > 0 then // When transmitting.
                st.transmitTime <- st.transmitTime - 1
            else if st.transmit && st.transmitTime = 0 then // When transmission finished.
                let pkt = packetPool.Value.Single(fun x -> ((fst x) = st.name) && ((snd x) = st.currentPacketNumber) && ((fourth x) = false))
                if (thd pkt).Value then // When collision occured.
                    st.backoff <- 500 * (rnd.Next(1, CW + 1)) + 8192 + 500
                    collisionCount <- collisionCount + 1
                    st.collision <- st.collision + 1
                    st.transmitTime <- -1
                    st.transmit <- false
                    sharedLine <- false
                else // When no collision.
                    //printfn "Successful packet transmission from station %s, packet number %d, time tick duration: [%d, %d]" (fst pkt) (snd pkt) (timeTick - 8192L) timeTick
                    if st.waitACK = -1 then // Now wait for ACK.
                        st.transmit <- false
                        st.transmitTime <- -1
                        sharedLine <- false
                        st.waitACK <- 8192 + 500
                packetPool.Value <- List.ofSeq(packetPool.Value.Except([pkt]))
            else // When not transmitting.
                if st.backoff > 0 then // When waiting for backoff.
                    st.ConsumeBackoff()
                else if st.backoff = 0 then // When backoff finished.
                    //if IsMediumIdle() then // When medium is idle, transmit.
                    if isIdleTick then
                        st.backoff <- -1
                        st.transmit <- true
                        st.transmitTime <- 8192
                        sharedLine <- true
                        st.packetTrial <- st.packetTrial + 1
                        packetPool.Value <- (st.name, st.currentPacketNumber, ref false, false) :: packetPool.Value
                else if st.IsPacketReady() then // When packet is ready and no backoff.
                    //if IsMediumIdle() then // When medium is idle, transmit.
                    if isIdleTick then
                        st.transmit <- true
                        st.transmitTime <- 8192
                        sharedLine <- true
                        st.packetTrial <- st.packetTrial + 1
                        st.packetStart <- timeTick
                        packetPool.Value <- (st.name, st.currentPacketNumber, ref false, false) :: packetPool.Value
                else // When packet is not ready.
                    st.ChargePacket()

        if (timeTick - lastCheckTime > 10000000L) then
            printfn "Current packetCount: %d, Current throughput: %f (packets/sec), Cumulated throughput: %f (packets/sec)" packetCount ((float)(packetCount - lastPacketCount) / ((float)(timeTick - lastCheckTime) / 10000000.0)) ((float)packetCount / ((float)timeTick / 10000000.0))
            //printfn "Current packetCount: %d" packetCount
            lastCheckTime <- timeTick
            lastPacketCount <- packetCount
        CheckCollision() |> ignore
        timeTick <- timeTick + 1L // Next simulated-time. (Equal to next unit time.)
    labelOutputResultThroughput.Text <- ((float)packetCount / ((float)(totalTime) / 10000000.0)).ToString()
    mainForm.Refresh()
    for st in stationPool.Value do
        packetTrialCount <- packetTrialCount + (int64)st.packetTrial
        perStationCollisionCount <- perStationCollisionCount + st.collision
    printfn "===================="
    printfn "End of simulation"
    printfn "Total packets transmitted: %d" packetCount
    printfn "Throughput: %f" ((float)packetCount / ((float)(totalTime) / 10000000.0))
    printfn "Total collisions: %d" collisionCount
    printfn "Total per-station collisions: %d" perStationCollisionCount
    printfn "Total transmission attempt: %d" packetTrialCount
    if packetCount = 0L then
        printfn "Mean packet dealy: 0"
    else
        printfn "Mean packet delay: %A" (packetDelay / Numerics.BigInteger(packetCount))
    printfn "Total Ticks: %d" timeTick
    printfn "Total idle ticks: %d" idleTicks
    printfn "Total busy ticks: %d" busyTicks
    printfn "===================="
    resultThroughput <- ((float)packetCount / ((float)(totalTime) / 10000000.0))
    if packetCount = 0L then
        resultMeanPacketDelay <- -1I
    else
        resultMeanPacketDelay <- (packetDelay / Numerics.BigInteger(packetCount))
    resultCollisionProbability <- (float)perStationCollisionCount / (float)packetTrialCount
    printfn "===================="
    printfn "Simulation Result: "
    printfn "    Throughput:                         %f" resultThroughput
    printfn "    Mean Packet Delay:                  %A" resultMeanPacketDelay
    printfn "    Transmission Collision Probability: %f" resultCollisionProbability
    printfn "===================="
)

buttonRunBinaryPST.Click.Add(fun _ ->
    printfn "===================="
    printfn "Simulation Info: "
    printfn "    Scheme:         1-persistent CSMA/CD + Binary-exponential backoff"
    printfn "    Node Count:     %d" nodeCount
    printfn "    Execution Time: %d (sec)" totalExecutionTime
    printfn "===================="
    printfn "Now starting simulation...\n"

    let rnd = System.Random()
    stationPool.Value <- [ for i in 1 .. nodeCount -> new station(i.ToString(), (0 - (int)(100000.0 * Math.Log(rnd.NextDouble())))) ]
    //for st in stationPool.Value do
        //st.chargeTime <- 500 - st.chargeTime % 500 + st.chargeTime // Set time to slot.
    let totalTime = (int64)totalExecutionTime * 10000000L // Total time to execute simulation. Unit : 1 unit time (100 ns).
    let startTime = 0L
    let mutable lastPacketCount = 0L
    let mutable lastCheckTime = 0L
    while (timeTick < totalTime) do
        let isIdleTick = IsMediumIdle()
        for st in stationPool.Value do
            if st.transmit && st.transmitTime > 0  then // When transmitting.
                st.transmitTime <- st.transmitTime - 1
                let isCollision = CheckCollision()
                if isCollision then
                    for pkt in packetPool.Value do
                        let pktStation = stationPool.Value.Single(fun x -> x.name = (fst pkt))
                        //pktStation.backoff <- 500 * ((int)((float)CW * rnd.NextDouble()))
                        pktStation.backoff <- 500 * rnd.Next(1, pktStation.binaryCW + 1)
                        pktStation.updateBinaryCW()
                        pktStation.transmitTime <- -1
                        pktStation.transmit <- false
                        pktStation.collision <- pktStation.collision + 1
                    collisionCount <- collisionCount + 1
                    packetPool.Value <- ([] : (string * int * bool ref * bool) list)
            else if st.transmit && st.transmitTime = 0 then // When transmission finished.
                let pkt = packetPool.Value.Single(fun x -> ((fst x) = st.name) && ((snd x) = st.currentPacketNumber) && ((fourth x) = false))
                //printfn "Successful packet transmission from station %s, packet number %d, time tick duration: [%d, %d]" (fst pkt) (snd pkt) (timeTick - 8192L) timeTick
                packetCount <- packetCount + 1L
                st.transmit <- false
                st.transmitTime <- -1
                sharedLine <- false
                st.packetEnd <- timeTick
                packetDelay <- packetDelay + Numerics.BigInteger(st.packetEnd - st.packetStart)
                st.binaryCW <- 2
                st.GeneratePacket()
                packetPool.Value <- List.ofSeq(packetPool.Value.Except([pkt]))
            else // When not transmitting.
                if st.backoff > 0 then // When waiting for backoff.
                    st.ConsumeBackoff()
                else if st.backoff = 0 then // When backoff finished.
                    //if IsMediumIdle() then // When medium is idle, transmit.
                    if isIdleTick then
                        st.backoff <- -1
                        st.transmit <- true
                        st.transmitTime <- 8192
                        sharedLine <- true
                        st.packetTrial <- st.packetTrial + 1
                        packetPool.Value <- (st.name, st.currentPacketNumber, ref false, false) :: packetPool.Value
                else if st.IsPacketReady() then // When packet is ready and no backoff.
                    //if IsMediumIdle() then // When medium is idle, transmit.
                    if isIdleTick then
                        st.transmit <- true
                        st.transmitTime <- 8192
                        sharedLine <- true
                        st.packetTrial <- st.packetTrial + 1
                        st.packetStart <- timeTick
                        packetPool.Value <- (st.name, st.currentPacketNumber, ref false, false) :: packetPool.Value
                else // When packet is not ready.
                    st.ChargePacket()

        if (timeTick - lastCheckTime > 10000000L) then
            printfn "Current packetCount: %d, Current throughput: %f (packets/sec), Cumulated throughput: %f (packets/sec)" packetCount ((float)(packetCount - lastPacketCount) / ((float)(timeTick - lastCheckTime) / 10000000.0)) ((float)packetCount / ((float)timeTick / 10000000.0))
            //printfn "Current packetCount: %d" packetCount
            lastCheckTime <- timeTick
            lastPacketCount <- packetCount

        timeTick <- timeTick + 1L // Next simulated-time. (Equal to next unit time.)
    labelOutputResultThroughput.Text <- ((float)packetCount / ((float)(totalTime) / 10000000.0)).ToString()
    mainForm.Refresh()
    for st in stationPool.Value do
        packetTrialCount <- packetTrialCount + (int64)st.packetTrial
        perStationCollisionCount <- perStationCollisionCount + st.collision
    printfn "===================="
    printfn "End of simulation"
    printfn "Total packets transmitted: %d" packetCount
    printfn "Throughput: %f" ((float)packetCount / ((float)(totalTime) / 10000000.0))
    printfn "Total collisions: %d" collisionCount
    printfn "Total per-station colisions: %d" perStationCollisionCount
    printfn "Total transmission attempt: %d" packetTrialCount
    if packetCount = 0L then
        printfn "Mean packet dealy: 0"
    else
        printfn "Mean packet delay: %A" (packetDelay / Numerics.BigInteger(packetCount))
    printfn "Total Ticks: %d" timeTick
    printfn "Total idle ticks: %d" idleTicks
    printfn "Total busy ticks: %d" busyTicks
    printfn "===================="
    resultThroughput <- ((float)packetCount / ((float)(totalTime) / 10000000.0))
    if packetCount = 0L then
        resultMeanPacketDelay <- -1I
    else
        resultMeanPacketDelay <- (packetDelay / Numerics.BigInteger(packetCount))
    resultCollisionProbability <- (float)perStationCollisionCount / (float)packetTrialCount
    printfn "===================="
    printfn "Simulation Result: "
    printfn "    Throughput:                         %f" resultThroughput
    printfn "    Mean Packet Delay:                  %A" resultMeanPacketDelay
    printfn "    Transmission Collision Probability: %f" resultCollisionProbability
    printfn "===================="
)

buttonReset.Click.Add(fun _ ->
    nodeCount <- 0
    totalExecutionTime <- 0
    CW <- 0
    timeTick <- 0L
    packetCount <- 0L
    collisionCount <- 0
    packetTrialCount <- 0L
    perStationCollisionCount <- 0
    packetDelay <- 0I
    packetPool.Value <- ([] : (string * int * bool ref * bool) list)
    idleTicks <- 0L
    busyTicks <- 0L
    resultThroughput <- 0.0
    resultMeanPacketDelay <- 0I
    resultCollisionProbability <- 0.0
    textboxNodeCount.Text <- ""
    textboxTotalTime.Text <- ""
    textboxCW.Text <- ""
    textboxAutoTestNodeCounts.Text <- ""
    textboxAutoTestExecutionTimes.Text <- ""
    textboxAutoTestCWs.Text <- ""
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

buttonAutoTest.Click.Add(fun _ ->
    printfn "Now performing automatic test..."
    printfn "===================="
    if (File.Exists("./AutoTestResult.csv")) then
        File.Delete("./AutoTestResult.csv")
    File.AppendAllText("./AutoTestResult.csv", "Scheme"+","+"NodeCount"+","+"ExecutionTime"+","+"CW"+","+"Result-Throughput"+","+"Result-MeanPacketDelay"+","+"Result-CollisionProbability"+System.Environment.NewLine)
    let mutable nodeCounts = [| 5; 10; |]    
    let mutable executionTimes = [| 1; |]
    let mutable CWs = [| 32; 64; |]
    if textboxAutoTestNodeCounts.Text <> "" then
        nodeCounts <- textboxAutoTestNodeCounts.Text.Replace(" ", "").Split([| ',' |]) |> Array.map (fun x -> Convert.ToInt32(x))
    if textboxAutoTestExecutionTimes.Text <> "" then
        executionTimes <- textboxAutoTestExecutionTimes.Text.Replace(" ", "").Split([| ',' |]) |> Array.map (fun x -> Convert.ToInt32(x))
    if textboxAutoTestCWs.Text <> "" then
        CWs <- textboxAutoTestCWs.Text.Replace(" ", "").Split([| ',' |]) |> Array.map (fun x -> Convert.ToInt32(x))
    for nc in nodeCounts do
        for et in executionTimes do
            for cw in CWs do
                if checkboxAutoTestUniformPST.Checked then
                    buttonReset.PerformClick()
                    textboxNodeCount.Text <- nc.ToString()
                    textboxTotalTime.Text <- et.ToString()
                    textboxCW.Text <- cw.ToString()
                    buttonNodeCountSet.PerformClick()
                    buttonTotalTimeSet.PerformClick()
                    buttonCWSet.PerformClick()
                    buttonRunUniformPST.PerformClick()
                    File.AppendAllText("./AutoTestResult.csv", "CSMA-Uniform"+","+nc.ToString()+","+et.ToString()+","+cw.ToString()+","+resultThroughput.ToString()+","+resultMeanPacketDelay.ToString()+","+resultCollisionProbability.ToString()+System.Environment.NewLine)
                if checkboxAutoTestUniformPSTCD.Checked then
                    buttonReset.PerformClick()
                    textboxNodeCount.Text <- nc.ToString()
                    textboxTotalTime.Text <- et.ToString()
                    textboxCW.Text <- cw.ToString()
                    buttonNodeCountSet.PerformClick()
                    buttonTotalTimeSet.PerformClick()
                    buttonCWSet.PerformClick()
                    buttonRunUniformPSTCD.PerformClick()
                    File.AppendAllText("./AutoTestResult.csv", "CSMA/CD-Uniform"+","+nc.ToString()+","+et.ToString()+","+cw.ToString()+","+resultThroughput.ToString()+","+resultMeanPacketDelay.ToString()+","+resultCollisionProbability.ToString()+System.Environment.NewLine)
                if checkboxAutoTestBinaryPST.Checked then
                    buttonReset.PerformClick()
                    textboxNodeCount.Text <- nc.ToString()
                    textboxTotalTime.Text <- et.ToString()
                    textboxCW.Text <- cw.ToString()
                    buttonNodeCountSet.PerformClick()
                    buttonTotalTimeSet.PerformClick()
                    buttonCWSet.PerformClick()
                    buttonRunBinaryPST.PerformClick()
                    File.AppendAllText("./AutoTestResult.csv", "CSMA/CD-Binary"+","+nc.ToString()+","+et.ToString()+","+cw.ToString()+","+resultThroughput.ToString()+","+resultMeanPacketDelay.ToString()+","+resultCollisionProbability.ToString()+System.Environment.NewLine)

    printfn "===================="
    printfn "End of automatic test."
)

// Add all controls to form.
mainForm.Controls.AddRange([| buttonNodeCountSet; buttonTotalTimeSet; buttonCWSet; buttonClose; buttonReset; buttonRunUniformPST; buttonRunUniformPSTCD; buttonRunBinaryPST; buttonAutoTest;
                                textboxNodeCount; textboxTotalTime; textboxCW; textboxAutoTestNodeCounts; textboxAutoTestExecutionTimes; textboxAutoTestCWs;
                                labelNodeCount; labelTotalTime; labelCW; labelCurrentRate; labelCumulatedRate; labelResultThroughput; labelUniformNodeCount; labelUniformTotalTime; labelUniformCW;
                                labelUniformCDNodeCount; labelUniformCDTotalTime; labelUniformCDCW; labelBinaryNodeCount; labelBinaryTotalTime;
                                labelOutputCurrentRate; labelOutputCumulatedRate; labelOutputResultThroughput; labelOutputUniformNodeCount; labelOutputUniformTotalTime; labelOutputUniformCW;
                                labelOutputUniformCDNodeCount; labelOutputUniformCDTotalTime; labelOutputUniformCDCW; labelOutputBinaryNodeCount; labelOutputBinaryTotalTime;
                                labelAutoTestNodeCounts; labelAutoTestExecutionTimes; labelAutoTestCWs; labelAutoTestUniformPST; labelAutoTestUniformPSTCD; labelAutoTestBinaryPST;
                                labelInfoUniform; labelInfoUniformCD; labelInfoBinary;
                                checkboxAutoTestUniformPST; checkboxAutoTestUniformPSTCD; checkboxAutoTestBinaryPST; |])

mainForm.ShowDialog() |> ignore

//
// End region for keep-updating labels.
// ---------------------

//
// End Region for GUI Form.
//