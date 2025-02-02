//
// Created by Jakub on 17.01.2025.
//

#ifndef CARTRIDGE_H
#define CARTRIDGE_H
#include <cstdint>
#include <string>

namespace CARTRIDGE {
    struct CartridgeHeader {
        uint8_t entry[4];
        uint8_t logo[0x30];
        int8_t title[16];
        uint16_t newLicensee;
        uint8_t sgbFlag;
        uint8_t cartridgeType;
        uint8_t romSize;
        uint8_t ramSize;
        uint8_t destination;
        uint8_t oldLicensee;
        uint8_t version;
        uint8_t checksum;
        uint16_t globalChecksum;
    };

    static const char *ROM_TYPES[] = {
        "ROM ONLY",
        "MBC1",
        "MBC1+RAM",
        "MBC1+RAM+BATTERY",
        "0x04 ???",
        "MBC2",
        "MBC2+BATTERY",
        "0x07 ???",
        "ROM+RAM 1",
        "ROM+RAM+BATTERY 1",
        "0x0A ???",
        "MMM01",
        "MMM01+RAM",
        "MMM01+RAM+BATTERY",
        "0x0E ???",
        "MBC3+TIMER+BATTERY",
        "MBC3+TIMER+RAM+BATTERY 2",
        "MBC3",
        "MBC3+RAM 2",
        "MBC3+RAM+BATTERY 2",
        "0x14 ???",
        "0x15 ???",
        "0x16 ???",
        "0x17 ???",
        "0x18 ???",
        "MBC5",
        "MBC5+RAM",
        "MBC5+RAM+BATTERY",
        "MBC5+RUMBLE",
        "MBC5+RUMBLE+RAM",
        "MBC5+RUMBLE+RAM+BATTERY",
        "0x1F ???",
        "MBC6",
        "0x21 ???",
        "MBC7+SENSOR+RUMBLE+RAM+BATTERY",
    };
}

class Cartridge {
    CARTRIDGE::CartridgeHeader *header;
    uint32_t romSize;
    uint32_t selectedRomBank;
    uint8_t *rom;
    uint32_t ramSize;
    uint32_t selectedRamBank;
    uint8_t *ram;
    std::string fileName;

public:
    explicit Cartridge( std::string fileName);

    ~Cartridge();

    bool load();

    void dump() const;

    uint8_t read(uint16_t address) const;
    void write(uint16_t address, uint8_t data);
};


#endif //CARTRIDGE_H
