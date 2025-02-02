//
// Created by Jakub on 17.01.2025.
//

#include "Bus.h"

#include <iostream>

BUS::Bus::Bus(Cartridge &cartridge):cartridge(&cartridge),selectedVramBank(0),interruptEnableRegisterer(0)  {
    interruptAddress.fill(0x00);
    romDataArea.fill(0x00);
    characterData.fill(0x00);
    displayData1.fill(0x00);
    displayData2.fill(0x00);
    wram.fill(0x00);
    objectAttributeMemory.fill(0x00);
    IOregister.fill(0x00);
    highRam.fill(0x00);
}

uint8_t BUS::Bus::read(const uint16_t address) const {
    if (address < 0x100) {
        return interruptAddress[address];
    }
    if (address < 0x8000) {
        return cartridge->read(address);
    }
    if (address < 0x9800) {
        return characterData[address - 0x8000];
    }
    if (address < 0x9C00) {
        return displayData1[address - 0x9800];
    }
    if (address < 0xA000) {
        return displayData2[address - 0x9C00];
    }
    if (address < 0xC000) {
        //8 KiB External RAM
        return cartridge->read(address);
    }
    if (address < 0xD000) {
        return wram[address - 0xC000];
    }
    if (address < 0xE000) {
        //4 KiB Work RAM (WRAM)
        return wram[address - 0xC000];
    }
    if (address < 0xFE00) {
        //Not Usable
        std::cout<<"Address Not Usable\n";
        return 0;
    }
    if (address < 0xFEA0) {
        //Object attribute memory (OAM)
        return objectAttributeMemory[address - 0xFE00];
    }
    if (address < 0xFF00) {
        //Not Usable
        std::cout<<"Address Not Usable\n";
        return 0;
    }
    if (address < 0xFF80) {
        return IOregister[address - 0xFF00];
    }
    if (address < 0xFFFF) {
        return highRam[address - 0xFF80];
    }

    return interruptEnableRegisterer;
}

void BUS::Bus::write(const uint16_t address, uint8_t value) {
    if (address < 0x100) {
        interruptAddress[address] = value;
        return;
    }
    if (address < 0x8000) {
        std::cout<<"Cant write to Rom\n";
        return;
    }
    if (address < 0x9800) {
        characterData[address - 0x8000] = value;
        return;
    }
    if (address < 0x9C00) {
        displayData1[address - 0x9800] = value;
        return;
    }
    if (address < 0xA000) {
        displayData2[address - 0x9C00] = value;
        return;
    }
    if (address < 0xC000) {
        cartridge->write(address,value);
        return;
    }
    if (address < 0xD000) {
        wram[address - 0xC000] = value;
        return;
    }
    if (address < 0xE000) {
        wram[address - 0xC000] = value;
        return;
    }
    if (address < 0xFE00) {
        std::cout<<"Address Not Usable\n";
        return;
    }
    if (address < 0xFEA0) {
        objectAttributeMemory[address - 0xFE00] = value;
        return;
    }
    if (address < 0xFF00) {
        std::cout<<"Address Not Usable\n";
        return;
    }
    if (address < 0xFF80) {
        IOregister[address - 0xFF00] = value;
        return;
    }
    if (address < 0xFFFF) {
        highRam[address - 0xFF80] = value;
        return;
    }

    //Interrupt Enable register (IE)
    interruptEnableRegisterer = value;

}
