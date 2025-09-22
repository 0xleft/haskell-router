import Test.Hspec ( hspec, describe, it, shouldBe )
import Router.Layers.Ethernet as Ethernet
import Router.Layers as Layers
import Router.Packet as Packet
import Router.Packer as Packer
import Foreign (free)
import Network.Pcap (openLive, sendPacket)
import Router.Layers.BeaconFrame as BeaconFrame

main :: IO ()
main = hspec $ do
  describe "Sanity test" $ do
    it "Shoud not fail" $ do
      True `shouldBe` True

  describe "BeaconFrame converters" $ do
    it "return the correct Word16 representation for beacon frame" $ do
      let fc = BeaconFrame.getFrameControl 8 0 0 False False False False False False False False
      fc `shouldBe` 0x8000

    it "retern to correct hex repr" $ do
      let s = BeaconFrame.getMacAddress "e6:cd:ac:8e:0e:35"
      s `shouldBe` [0xe6, 0xcd, 0xac, 0x8e, 0x0e, 0x35]
      
    it "Test MACHeader builder" $ do
      let mac = BeaconFrame.setMacHeaderBSsid "00:00:00:00:00:08"
                . BeaconFrame.setMacHeaderDuration 10
                . BeaconFrame.setMacHeaderDestination "ff:ff:ff:ff:ff:ff"
                . BeaconFrame.setMacHeaderSeqControl 1 0
                $ BeaconFrame.defaultMacHeader
      (BeaconFrame.bSsidMacAddress mac) `shouldBe` [0,0,0,0,0,0x08]
      (BeaconFrame.destinationMacAddress mac) `shouldBe` [255,255,255,255,255,255]
      (BeaconFrame.duration mac) `shouldBe` 10
      (BeaconFrame.seqControl mac) `shouldBe` 0x0010
    
    it "Test FrameBody builder" $ do
      let body = BeaconFrame.setFrameBodyBeaconInterval 5000 
                  . BeaconFrame.setFrameBodyCapabilityInfo 0x1501
                  . BeaconFrame.setFrameBodySSID "Super Awesome Network"
                  . BeaconFrame.setFrameBodyTimeStamp 12347890
                  . BeaconFrame.setFrameBodySupportedRates [1,100,69]
                  $ defaultFrameBody
      (BeaconFrame.tagLength (ssid body)) `shouldBe` 21
      (BeaconFrame.ssidName (ssid body)) `shouldBe` "Super Awesome Network"
      (BeaconFrame.allSupportedRates (supportedRates body)) `shouldBe` [0x01+128, 0x64+128, 0x45+128]
      (BeaconFrame.capabilityInfo body) `shouldBe` 0x1501
      (BeaconFrame.timeStamp body) `shouldBe` 0xbc69f2

                


  describe "Should test packet packer" $ do
    it "Should return if packet has parent and not correctly" $ do
      let eth = Layers.Ethernet [] [] Ethernet.IPv4Packet
          ethernetLayer = Layers.PacketLinkLayer (Layers.LinkLayerEth eth)
          packet = Packet.Packet ethernetLayer

      (Packer.hasField "sourceMac" ethernetLayer) `shouldBe` True
      (Packer.hasField "notexistantfield" ethernetLayer) `shouldBe` False
    
    it "Should " $ do -- todo actualy finish this test
      let eth = Layers.Ethernet [0, 0, 0, 0, 0, 0] [0, 0, 0, 0, 0, 0] Ethernet.IPv4Packet
          ethernetLayer = Layers.PacketLinkLayer (Layers.LinkLayerEth eth)
          ip = Layers.Ip 0x45 0 0 0 0 0 0 0 0 0 [] ethernetLayer
          ipLayer = Layers.PacketNetworkLayer (Layers.NetworkLayerIp ip)
          tcp = Layers.Tcp 0 0 0 0 0 0 0 0 0 0 [] ipLayer
          tcpLayer = Layers.PacketTransferLayer (Layers.TransferLayerTcp tcp)

          packet = Packet.Packet tcpLayer
          fields = Packer.getFields packet
          recursiveFields = Packer.getRecursiveFields (topLayer packet)
          convertedFields = Packer.convertFields recursiveFields 

      (ptr, len, ws) <- Packer.pack packet

      putStrLn $ Prelude.show ws

      -- interface <- openLive "wlo1" 65535 False 0
      -- sendPacket interface ptr len

      free ptr

      -- _ <- putStrLn (Prelude.show fields)
      True `shouldBe` True