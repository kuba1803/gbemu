//
// Created by Jakub on 17.01.2025.
//

#ifndef BUS_H
#define BUS_H
#include <array>
#include <cstdint>

#include "Cartridge.h"


namespace BUS {
    enum specialRegisterAddress {
        P1 = 0xFF00,
        SB = 0xFF01,
        SC = 0xFF02,
        DIV = 0xFF04,
        TIMA = 0xFF05,
        TMA = 0xFF06,
        TAC = 0xFF07,
        KEY1 = 0xFF4D,
        RP = 0xFF56,
        VBK = 0xFF4F,
        SVBK = 0xFF70,
        IF = 0xFF0F,
        IE = 0xFFFF,
        LCDC = 0xFF40,
        STAT = 0xFF41,
        SCY = 0xFF42,
        SCX = 0xFF43,
        LY = 0xFF44,
        LYC = 0xFF45,
        DMA = 0xFF46,
        BGP = 0xFF47,
        OBP0 = 0xFF48,
        OBP1 = 0xFF49,
        WY = 0xFF4A,
        WX = 0xFF4B,
        HDMA1 = 0xFF51,
        HDMA2 = 0xFF52,
        HDMA3 = 0xFF53,
        HDMA4 = 0xFF54,
        HDMA5 = 0xFF55,
        BCPS = 0xFF68,
        BCPD = 0xFF69,
        OCPS = 0xFF6A,
        OCPD = 0xFF6B
    };

    class Bus {
        Cartridge* cartridge;
        uint8_t selectedVramBank;
        std::array<uint8_t, 0x100> interruptAddress{};
        std::array<uint8_t, 0x150> romDataArea{};
        std::array<uint8_t, 0x3000> characterData{};
        std::array<uint8_t, 0x800> displayData1{};
        std::array<uint8_t, 0x800> displayData2{};
        std::array<uint8_t, 0x8000> wram{};
        std::array<uint8_t, 0xA0> objectAttributeMemory{};
        std::array<uint8_t, 0xA0> IOregister{};
        std::array<uint8_t, 0xA0> highRam{};
        uint8_t interruptEnableRegisterer;

    public:
        explicit Bus(Cartridge &cartridge);

        [[nodiscard]] uint8_t read(uint16_t address) const;
        void write(uint16_t address, uint8_t data);
    };
}



#endif //BUS_H
