//
// Created by Jakub on 17.01.2025.
//

#include "Cartridge.h"

#include <format>
#include <iostream>
#include <ostream>
#include <utility>


Cartridge::Cartridge( std::string fileName): header(nullptr), romSize(0), selectedRomBank(0), rom(nullptr), ramSize(0),
                                             selectedRamBank(0),
                                             ram(nullptr),
                                             fileName(std::move(fileName)) {
}

Cartridge::~Cartridge() {
    delete [] rom;
    delete [] ram;
}


bool Cartridge::load() {
    std::FILE *file = std::fopen(fileName.c_str(), "r");
    if (!file) {
        std::cout<<"Failed to open file "<<fileName<<"\n";
        return false;
    }

    std::cout<<"Loaded file "<<fileName<<"\n";

    fseek(file, 0, SEEK_END);
    romSize = ftell(file);

    rewind(file);

    rom = new uint8_t[romSize];
    fread(rom, romSize, 1, file);
    fclose(file);
    header = reinterpret_cast<CARTRIDGE::CartridgeHeader *>(rom + 0x100);
    switch (header->ramSize) {
        case 2:
            ramSize = 8<<10;
            ram = new uint8_t[ramSize];
            break;
        case 3:
            ramSize = 32<<10;
            ram = new uint8_t[ramSize];
            break;
        case 4:
            ramSize = 128<<10;
            ram = new uint8_t[ramSize];
            break;
        case 5:
            ramSize = 64<<10;
            ram = new uint8_t[ramSize];
            break;
        default:
            ramSize = 0;
            break;
    }
    selectedRamBank = 1;
    selectedRomBank = 0;

    uint8_t  checkSum = 0;
    for ( uint16_t i = 0x0134; i <= 0x014C; i++) {
        checkSum = checkSum - rom[i] - 1;
    }

    dump();

    std::cout<< std::format("\t Checksum : {}  ({})\n", header->checksum,((checkSum == header->checksum)?"PASS":"FAILED") );

    return true;

}

void Cartridge::dump() const {
    std::cout<<"Cartridge Loaded:\n";
    std::cout<< std::format("\t Title    : {}\n", reinterpret_cast<char*>(header->title));
    std::cout<< std::format("\t Type     : {} ({})\n", header->cartridgeType, (header->cartridgeType<=34)?CARTRIDGE::ROM_TYPES[header->cartridgeType]:"UNKNOWN");
    std::cout<< std::format("\t ROM Size : {} KB\n", 32 << header->romSize);
    std::cout<< std::format("\t RAM Size : {}\n", header->ramSize);
    std::cout<< std::format("\t LIC Code : {} \n", header->newLicensee);
    std::cout<< std::format("\t ROM Vers : {}\n", header->version);
}


uint8_t Cartridge::read(uint16_t address) const {
    return 0;
}

void Cartridge::write(uint16_t address, uint8_t value) {

}


