{-# LANGUAGE DuplicateRecordFields #-}
module Types () where

data LinkLayer = LinkLayerEth Ethernet | LinkLayerWifi Wifi
data NetworkLayer = NetworkLayerIP IP
data TransferLayer = TransferLayerUDP UDP | TransferLayerTCP TCP
data ApplicationLayer = ApplicationLayerTxt Txt | ApplicationLayerHtml Html

data Ethernet = Ethernet {
    sourceMac :: String
}

data Wifi = Wifi {

}

data IP = IP {

    parent :: LinkLayer 
}

data TCP = TCP {
}

data UDP = UDP {

    parent :: NetworkLayer
}

data Txt = Txt {

    parent :: TransferLayer
}


data Html = Html {

    parent :: TransferLayer
}