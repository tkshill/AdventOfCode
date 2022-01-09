namespace Aoc2021.day16
module Day16 =
    open System
    
    // DOMAIN
   
    type Packet = { Version: int; Payload: Expr }
    and Expr = Literal of int64 | Operation of int * Packet seq
    
    type Limit = CountLimit | LengthLimit
    
    // HELPER FUNCS
    
    let intFromBase (base_:int) s = Convert.ToInt32(s.ToString(), base_)    
    let int64FromBase (b: int) s = Convert.ToInt64(s.ToString(), b)    
    let intToBase (base_:int) (i:int) = Convert.ToString(i, base_)
    let splitString n str = String.concat "" (Seq.take n str), String.concat "" (Seq.skip n str)

    // PART 1
    
    // takes our input and transforms it into a big binary string
    let hexesToBinary str =
        let rec addZeroes s = if String.length s = 4 then s else addZeroes ("0" + s)
        
        Seq.map (intFromBase 16 >> intToBase 2 >> addZeroes) str |> String.concat ""
    
    // transforms a string of bits to a number and remaining strings
    let toLiteral strings =
        let binaryChunk = function
            | "1" :: a :: b :: c :: d :: rest -> Some( $"{a}{b}{c}{d}", rest )
            | "0" :: a :: b :: c :: d :: rest -> Some( $"{a}{b}{c}{d}", "stop" :: rest )
            | "stop" :: rest -> Some( String.concat "" rest, List.empty )
            | _ -> None
        
        let digits, remainder =
            List.unfold binaryChunk strings
            |> fun values -> (List.rev >> List.tail >> List.rev) values , List.last values
            
        let result = List.fold (+) "" digits |> fun s -> Convert.ToInt64(s.ToString(), 2)
        
        result, remainder
    
    let (|LiteralType|OperationType|) = function | 4 -> LiteralType | _ -> OperationType
    
    // converts a string of bits to a packet
    let rec toPacket str =
        let v1 :: v2 :: v3 :: t1 :: t2 :: t3 :: rest = (Seq.toList >> List.map string) str

        let version = intFromBase 2 $"{v1}{v2}{v3}"
        let type_ = intFromBase 2 $"{t1}{t2}{t3}"
        
        match type_ with
        | LiteralType ->
            let literal, remainder = toLiteral rest
            
            { Version = version; Payload = Literal literal }, remainder
            
        | OperationType ->
            let subPackets, remainder = toLimit (Seq.head rest) |> toSubPackets (Seq.tail rest)
            
            { Version = version; Payload = Operation (type_, subPackets) }, remainder
                
    and limitToFunc = function | CountLimit -> tryPacketCount | LengthLimit -> tryPacketLength
    and toLimit = function | "1" -> CountLimit | _ -> LengthLimit
    and limitToSize = function | CountLimit -> 11 | LengthLimit -> 15
                
    and toSubPackets str limitType  =
        let limitBits, rest = splitString (limitToSize limitType) str
        let total = intFromBase 2 limitBits
        let packetsRaw = Seq.unfold ((limitToFunc limitType) total) (0, rest)
        
        (Seq.map fst packetsRaw), (Seq.last >> snd) packetsRaw

    and tryPacketCount total (completed, str) =
        if completed = total then None
        else
            let packet, remainder = toPacket str
            Some((packet, remainder), (completed + 1, remainder))
            
    and tryPacketLength total (consumed, (str:string)) =
        if consumed = total then None
        else
            let packet, remainder = toPacket str
            let updatedConsumed = consumed + str.Length - remainder.Length
            Some((packet, remainder), (updatedConsumed, remainder))
    
    // counts all versions in a packet and its subpackets
    let rec countVersions = function
        | { Version = version; Payload = Literal _ } -> version
        | { Version = version; Payload = Operation (_, packets) } -> version + Seq.sumBy countVersions packets
    
    // PART 2
    
    // collapses a packet into an integer based on the expressions for each operation
    let rec collapse = function
        | { Payload = Literal value } -> value
        | { Payload = Operation (type_, packets) } ->
            match type_ with
            | 0 -> Seq.sumBy collapse packets
            | 1 -> (Seq.map collapse >> Seq.fold (*) 1L) packets
            | 2 -> collapse (Seq.minBy collapse packets) 
            | 3 -> collapse (Seq.maxBy collapse packets)
            | 5 -> if collapse (Seq.head packets) > collapse (Seq.last packets) then 1L else 0L
            | 6 -> if collapse (Seq.head packets) < collapse (Seq.last packets) then 1L else 0L
            | 7 -> if collapse (Seq.head packets) = collapse (Seq.last packets) then 1L else 0L
   
module Day16test =
    
    open FsUnit.Xunit
    open Xunit
    open Day16
    
    [<Theory>]
    [<InlineData("EE00D40C823060", 14)>]
    [<InlineData("8A004A801A8002F478", 16)>]
    [<InlineData("620080001611562C8802118E34", 12)>]
    [<InlineData("C0015000016115A2E0802F182340", 23)>]
    [<InlineData("A0016C880162017C3686B18A3D4780", 31)>]
    let ``Given a string of hex numbers, packets produce the valid version sum`` (input, expected) =
        (hexesToBinary >> toPacket >> fst >> countVersions) input
        |> should equal expected
        
    [<Theory>]
    [<InlineData("C200B40A82", 3L)>]
    [<InlineData("04005AC33890", 54L)>]
    [<InlineData("880086C3E88112", 7L)>]
    [<InlineData("CE00C43D881120", 9L)>]
    [<InlineData("D8005AC2A8F0", 1L)>]
    [<InlineData("F600BC2D8F", 0L)>]
    [<InlineData("9C005AC2F8F0", 0L)>]
    [<InlineData("9C0141080250320F1802104A08", 1L)>]
    let ``Given a string of hex numbers, operations map to the correct values`` (input, expected) =
        (hexesToBinary >> toPacket >> fst >> collapse) input
        |> should equal expected
    
    [<Fact>]
    let ``Day 16 part 1`` () =
        let input = "220D790065B2745FF004672D99A34E5B33439D96CEC80373C0068663101A98C406A5E7395DC1804678BF25A4093BFBDB886CA6E11FDE6D93D16A100325E5597A118F6640600ACF7274E6A5829B00526C167F9C089F15973C4002AA4B22E800FDCFD72B9351359601300424B8C9A00BCBC8EE069802D2D0B945002AB2D7D583E3F00016B05E0E9802BA00B4F29CD4E961491CCB44C6008E80273C393C333F92020134B003530004221347F83A200D47F89913A66FB6620016E24A007853BE5E944297AB64E66D6669FCEA0112AE06009CAA57006A0200EC258FB0440010A8A716A321009DE200D44C8E31F00010887B146188803317A3FC5F30056C0150004321244E88C000874468A91D2291802B25EB875802B28D13550030056C0169FB5B7ECE2C6B2EF3296D6FD5F54858015B8D730BB24E32569049009BF801980803B05A3B41F1007625C1C821256D7C848025DE0040E5016717247E18001BAC37930E9FA6AE3B358B5D4A7A6EA200D4E463EA364EDE9F852FF1B9C8731869300BE684649F6446E584E61DE61CD4021998DB4C334E72B78BA49C126722B4E009C6295F879002093EF32A64C018ECDFAF605989D4BA7B396D9B0C200C9F0017C98C72FD2C8932B7EE0EA6ADB0F1006C8010E89B15A2A90021713610C202004263E46D82AC06498017C6E007901542C04F9A0128880449A8014403AA38014C030B08012C0269A8018E007A801620058003C64009810010722EC8010ECFFF9AAC32373F6583007A48CA587E55367227A40118C2AC004AE79FE77E28C007F4E42500D10096779D728EB1066B57F698C802139708B004A5C5E5C44C01698D490E800B584F09C8049593A6C66C017100721647E8E0200CC6985F11E634EA6008CB207002593785497652008065992443E7872714"
        (hexesToBinary >> toPacket >> fst >> countVersions) input
        |> should equal "the answer"
        
    [<Fact>]
    let ``Day 16 part 2`` () =
        let input = "220D790065B2745FF004672D99A34E5B33439D96CEC80373C0068663101A98C406A5E7395DC1804678BF25A4093BFBDB886CA6E11FDE6D93D16A100325E5597A118F6640600ACF7274E6A5829B00526C167F9C089F15973C4002AA4B22E800FDCFD72B9351359601300424B8C9A00BCBC8EE069802D2D0B945002AB2D7D583E3F00016B05E0E9802BA00B4F29CD4E961491CCB44C6008E80273C393C333F92020134B003530004221347F83A200D47F89913A66FB6620016E24A007853BE5E944297AB64E66D6669FCEA0112AE06009CAA57006A0200EC258FB0440010A8A716A321009DE200D44C8E31F00010887B146188803317A3FC5F30056C0150004321244E88C000874468A91D2291802B25EB875802B28D13550030056C0169FB5B7ECE2C6B2EF3296D6FD5F54858015B8D730BB24E32569049009BF801980803B05A3B41F1007625C1C821256D7C848025DE0040E5016717247E18001BAC37930E9FA6AE3B358B5D4A7A6EA200D4E463EA364EDE9F852FF1B9C8731869300BE684649F6446E584E61DE61CD4021998DB4C334E72B78BA49C126722B4E009C6295F879002093EF32A64C018ECDFAF605989D4BA7B396D9B0C200C9F0017C98C72FD2C8932B7EE0EA6ADB0F1006C8010E89B15A2A90021713610C202004263E46D82AC06498017C6E007901542C04F9A0128880449A8014403AA38014C030B08012C0269A8018E007A801620058003C64009810010722EC8010ECFFF9AAC32373F6583007A48CA587E55367227A40118C2AC004AE79FE77E28C007F4E42500D10096779D728EB1066B57F698C802139708B004A5C5E5C44C01698D490E800B584F09C8049593A6C66C017100721647E8E0200CC6985F11E634EA6008CB207002593785497652008065992443E7872714"
        (hexesToBinary >> toPacket >> fst >> collapse) input
        |> should equal "the answer"
