//
// Created by Jakub on 16.01.2025.
//

#include "Cpu.h"


CPU::Cpu::Cpu(BUS::Bus &bus): registers({0, 0, 0, 0, 0, 0, 0, 0}) {
    this->bus = &bus;
    cycleNumber = 0;
    cycleToExecute = 0;
    interruptEnabled = false;
    haltMode = false;
}


template<CPU::OPERATION_TYPE type>
    requires (type == CPU::OPERATION_TYPE::ADD ||
              type == CPU::OPERATION_TYPE::ADC ||
              type == CPU::OPERATION_TYPE::SUB ||
              type == CPU::OPERATION_TYPE::SBC ||
              type == CPU::OPERATION_TYPE::AND ||
              type == CPU::OPERATION_TYPE::OR ||
              type == CPU::OPERATION_TYPE::XOR ||
              type == CPU::OPERATION_TYPE::CP ||
              type == CPU::OPERATION_TYPE::INC ||
              type == CPU::OPERATION_TYPE::DEC)
constexpr uint8_t arithmetic_8(const uint8_t x, const uint8_t y, CPU::Registers &registers) {
    uint16_t result = 0;
    if constexpr (type == CPU::OPERATION_TYPE::ADD) {
        result = x + y;
        registers.h = (x & 0x0F) + (y & 0x0F) > 0x0F;
        registers.n = false;
        registers.cy = result > 0xFF;
        registers.z = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::ADC) {
        result = x + y + registers.cy;
        registers.h = (x & 0x0F) + (y & 0x0F) + registers.cy > 0x0F;
        registers.n = false;
        registers.cy = result > 0xFF;
        registers.z = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SUB) {
        result = x - y;
        registers.h = (x & 0x0F) < (y & 0x0F);
        registers.n = true;
        registers.cy = x < y;
        registers.z = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SBC) {
        result = x - y - registers.cy;
        registers.h = (x & 0x0F) < (y & 0x0F) + registers.cy;
        registers.n = true;
        registers.cy = x < y + registers.cy;
        registers.z = result == 0;
    }
    if constexpr (type == CPU::OPERATION_TYPE::AND) {
        result = x & y;
        registers.h = true;
        registers.n = false;
        registers.cy = false;
        registers.z = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::OR) {
        result = x | y;
        registers.h = false;
        registers.n = false;
        registers.cy = false;
        registers.z = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::XOR) {
        result = x ^ y;
        registers.h = false;
        registers.n = false;
        registers.cy = false;
        registers.z = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::CP) {
        result = x;
        registers.h = (x & 0x0F) < (y & 0x0F);
        registers.n = true;
        registers.cy = x < y;
        registers.z = x - y == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::INC) {
        result = x + 1;
        registers.h = (x & 0x0F) + 1 > 0x0F;
        registers.n = false;
        registers.cy = result > 0xFF;
        registers.z = x - y == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::DEC) {
        result = x - 1;
        registers.h = (x & 0x0F) < 1;
        registers.n = true;
        registers.cy = x < 1;
        registers.z = x - y == 0;
    }
    return static_cast<uint8_t>(result);
}

template<CPU::OPERATION_TYPE type>
    requires (type == CPU::OPERATION_TYPE::ADD ||
              type == CPU::OPERATION_TYPE::INC ||
              type == CPU::OPERATION_TYPE::DEC)
constexpr uint16_t arithmetic_16(const uint16_t x, const uint16_t y, CPU::Registers &registers) {
    uint32_t result = 0;

    if constexpr (type == CPU::OPERATION_TYPE::ADD) {
        result = x + y;
        registers.h = (x & 0x0FFF) + (y & 0x0FFF) > 0x0FFF;
        registers.n = false;
        registers.cy = result > 0xFFFF;
    }

    if constexpr (type == CPU::OPERATION_TYPE::INC) {
        result = x + 1;
    }

    if constexpr (type == CPU::OPERATION_TYPE::DEC) {
        result = x - 1;
    }

    return static_cast<uint16_t>(result);
}

template<CPU::OPERATION_TYPE type>
    requires (type == CPU::OPERATION_TYPE::RLC ||
              type == CPU::OPERATION_TYPE::RL ||
              type == CPU::OPERATION_TYPE::RRC ||
              type == CPU::OPERATION_TYPE::RR ||
              type == CPU::OPERATION_TYPE::SLA ||
              type == CPU::OPERATION_TYPE::SRA ||
              type == CPU::OPERATION_TYPE::SRL ||
              type == CPU::OPERATION_TYPE::SWAP)
constexpr uint8_t shift_rotation(const uint8_t x, CPU::Registers &registers) {
    uint16_t result = 0;

    if constexpr (type == CPU::OPERATION_TYPE::RLC) {
        result = x << 1;
        registers.cy = result > 0xFF;
        result = result + registers.cy;
        registers.z = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::RL) {
        result = x << 1;
        result = result + registers.cy;
        registers.cy = result > 0xFF;
        registers.z = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::RRC) {
        registers.cy = x & 1;
        result = x >> 1;
        result = result + registers.cy << 7;
        registers.z = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::RR) {
        result = x + registers.cy << 8;
        registers.cy = x & 1;
        result = result >> 1;
        registers.z = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SLA) {
        result = x << 1;
        registers.cy = result > 0xFF;
        registers.z = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SRA) {
        registers.cy = x & 1;
        result = x >> 1 + x & 80;
        registers.z = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SRL) {
        registers.cy = x & 1;
        result = x >> 1;
        registers.z = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SWAP) {
        result = x >> 4 + x << 4;
        registers.z = result && 0xFF == 0;
    }

    return static_cast<uint8_t>(result);
}

template<CPU::OPERATION_TYPE type>
    requires (type == CPU::OPERATION_TYPE::BIT ||
              type == CPU::OPERATION_TYPE::SET ||
              type == CPU::OPERATION_TYPE::RES)
constexpr uint8_t bit_operation(const uint8_t n, const uint8_t &x, CPU::Registers &registers) {
    uint8_t value = x;
    if constexpr (type == CPU::OPERATION_TYPE::BIT) {
        registers.n = false;
        registers.h = false;
        registers.z = (x & (1 << (n - 1))) == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SET) {
        value = x | (1 << (n - 1));
    }

    if constexpr (type == CPU::OPERATION_TYPE::RES) {
        value = x & ~(1 << (n - 1));
    }

    return value;
}

void CPU::Cpu::executeCycle() {
    std::cout << "Execute Cycle:" << cycleNumber << "\n";
    uint16_t helpVariable16;
    uint8_t helpVariable8;
    uint8_t instruction;
    if (!cycleToExecute) {
        instruction = bus->read(registers.PC);
        switch (instruction) {
#pragma region 8_BIT_TRANSFER
            //LD A A
            case 0b01111111:
                ++registers.PC;
                break;
            //LD A B
            case 0b01111000:
                registers.reg_8[A] = registers.reg_8[B];
                break;
            //LD A C
            case 0b01111001:
                registers.reg_8[A] = registers.reg_8[C];
                ++registers.PC;
                break;
            //LD A D
            case 0b01111010:
                registers.reg_8[A] = registers.reg_8[D];
                ++registers.PC;
                break;
            //LD A E
            case 0b01111011:
                registers.reg_8[A] = registers.reg_8[E];
                ++registers.PC;
                break;
            //LD A H
            case 0b01111100:
                registers.reg_8[A] = registers.reg_8[H];
                ++registers.PC;
                break;
            //LD A L
            case 0b01111101:
                registers.reg_8[A] = registers.reg_8[L];
                ++registers.PC;
                break;
            //LD B A
            case 0b01000111:
                registers.reg_8[B] = registers.reg_8[A];
                ++registers.PC;
                break;
            //LD B B
            case 0b01000000:
                registers.reg_8[B] = registers.reg_8[B];
                ++registers.PC;
                break;
            //LD B C
            case 0b01000001:
                registers.reg_8[B] = registers.reg_8[C];
                ++registers.PC;
                break;
            //LD B D
            case 0b01000010:
                registers.reg_8[B] = registers.reg_8[D];
                ++registers.PC;
                break;
            //LD B E
            case 0b01000011:
                registers.reg_8[B] = registers.reg_8[E];
                ++registers.PC;
                break;
            //LD B H
            case 0b01000100:
                registers.reg_8[B] = registers.reg_8[H];
                ++registers.PC;
                break;
            //LD B L
            case 0b01000101:
                registers.reg_8[B] = registers.reg_8[L];
                ++registers.PC;
                break;
            //LD C A
            case 0b01001111:
                registers.reg_8[C] = registers.reg_8[A];
                ++registers.PC;
                break;
            //LD C B
            case 0b01001000:
                registers.reg_8[C] = registers.reg_8[B];
                ++registers.PC;
                break;
            //LD C C
            case 0b01001001:
                registers.reg_8[C] = registers.reg_8[C];
                ++registers.PC;
                break;
            //LD C D
            case 0b01001010:
                registers.reg_8[C] = registers.reg_8[D];
                ++registers.PC;
                break;
            //LD C E
            case 0b01001011:
                registers.reg_8[C] = registers.reg_8[E];
                ++registers.PC;
                break;
            //LD C H
            case 0b01001100:
                registers.reg_8[C] = registers.reg_8[H];
                ++registers.PC;
                break;
            //LD C L
            case 0b01001101:
                registers.reg_8[C] = registers.reg_8[L];
                ++registers.PC;
                break;
            //LD D A
            case 0b01010111:
                registers.reg_8[D] = registers.reg_8[A];
                ++registers.PC;
                break;
            //LD D B
            case 0b01010000:
                registers.reg_8[D] = registers.reg_8[B];
                ++registers.PC;
                break;
            //LD D C
            case 0b01010001:
                registers.reg_8[D] = registers.reg_8[C];
                ++registers.PC;
                break;
            //LD D D
            case 0b01010010:
                registers.reg_8[D] = registers.reg_8[D];
                ++registers.PC;
                break;
            //LD D E
            case 0b01010011:
                registers.reg_8[D] = registers.reg_8[E];
                ++registers.PC;
                break;
            //LD D H
            case 0b01010100:
                registers.reg_8[D] = registers.reg_8[H];
                ++registers.PC;
                break;
            //LD D L
            case 0b01010101:
                registers.reg_8[D] = registers.reg_8[L];
                ++registers.PC;
                break;
            //LD E A
            case 0b01011111:
                registers.reg_8[E] = registers.reg_8[A];
                ++registers.PC;
                break;
            //LD E B
            case 0b01011000:
                registers.reg_8[E] = registers.reg_8[B];
                ++registers.PC;
                break;
            //LD E C
            case 0b01011001:
                registers.reg_8[E] = registers.reg_8[C];
                ++registers.PC;
                break;
            //LD E D
            case 0b01011010:
                registers.reg_8[E] = registers.reg_8[D];
                ++registers.PC;
                break;
            //LD E E
            case 0b01011011:
                registers.reg_8[E] = registers.reg_8[E];
                ++registers.PC;
                break;
            //LD E H
            case 0b01011100:
                registers.reg_8[E] = registers.reg_8[H];
                ++registers.PC;
                break;
            //LD E L
            case 0b01011101:
                registers.reg_8[E] = registers.reg_8[L];
                ++registers.PC;
                break;
            //LD H A
            case 0b01100111:
                registers.reg_8[H] = registers.reg_8[A];
                ++registers.PC;
                break;
            //LD H B
            case 0b01100000:
                registers.reg_8[H] = registers.reg_8[B];
                ++registers.PC;
                break;
            //LD H C
            case 0b01100001:
                registers.reg_8[H] = registers.reg_8[C];
                ++registers.PC;
                break;
            //LD H D
            case 0b01100010:
                registers.reg_8[H] = registers.reg_8[D];
                ++registers.PC;
                break;
            //LD H E
            case 0b01100011:
                registers.reg_8[H] = registers.reg_8[E];
                ++registers.PC;
                break;
            //LD H H
            case 0b01100100:
                registers.reg_8[H] = registers.reg_8[H];
                ++registers.PC;
                break;
            //LD H L
            case 0b01100101:
                registers.reg_8[H] = registers.reg_8[L];
                ++registers.PC;
                break;
            //LD L A
            case 0b01101111:
                registers.reg_8[L] = registers.reg_8[A];
                ++registers.PC;
                break;
            //LD L B
            case 0b01101000:
                registers.reg_8[L] = registers.reg_8[B];
                ++registers.PC;
                break;
            //LD L C
            case 0b01101001:
                registers.reg_8[L] = registers.reg_8[C];
                ++registers.PC;
                break;
            //LD L D
            case 0b01101010:
                registers.reg_8[L] = registers.reg_8[D];
                ++registers.PC;
                break;
            //LD L E
            case 0b01101011:
                registers.reg_8[L] = registers.reg_8[E];
                ++registers.PC;
                break;
            //LD L H
            case 0b01101100:
                registers.reg_8[L] = registers.reg_8[H];
                ++registers.PC;
                break;
            //LD L L
            case 0b01101101:
                registers.reg_8[L] = registers.reg_8[L];
                ++registers.PC;
                break;
            //LD A n
            case 0b00111110:
                ++registers.PC;
                registers.reg_8[A] = bus->read(registers.PC);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD B n
            case 0b00000110:
                ++registers.PC;
                registers.reg_8[B] = bus->read(registers.PC);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD C n
            case 0b00001110:
                ++registers.PC;
                registers.reg_8[C] = bus->read(registers.PC);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD D n
            case 0b00010110:
                ++registers.PC;
                registers.reg_8[D] = bus->read(registers.PC);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD E n
            case 0b00011110:
                ++registers.PC;
                registers.reg_8[E] = bus->read(registers.PC);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD H n
            case 0b00100110:
                ++registers.PC;
                registers.reg_8[H] = bus->read(registers.PC);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD L n
            case 0b00101110:
                ++registers.PC;
                registers.reg_8[L] = bus->read(registers.PC);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD A (HL)
            case 0b01111110:
                registers.reg_8[A] = bus->read(registers.HL);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD B (HL)
            case 0b01000110:
                registers.reg_8[B] = bus->read(registers.HL);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD C (HL)
            case 0b01001110:
                registers.reg_8[C] = bus->read(registers.HL);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD D (HL)
            case 0b01010110:
                registers.reg_8[D] = bus->read(registers.HL);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD E (HL)
            case 0b01011110:
                registers.reg_8[E] = bus->read(registers.HL);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD H (HL)
            case 0b01100110:
                registers.reg_8[H] = bus->read(registers.HL);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD L (HL)
            case 0b01101110:
                registers.reg_8[L] = bus->read(registers.HL);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD (HL) A
            case 0b01110111:
                bus->write(registers.HL, A);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD (HL) B
            case 0b01110000:
                bus->write(registers.HL, B);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD (HL) C
            case 0b01110001:
                bus->write(registers.HL, C);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD (HL) D
            case 0b01110010:
                bus->write(registers.HL, D);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD (HL) E
            case 0b01110011:
                bus->write(registers.HL, E);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD (HL) H
            case 0b01110100:
                bus->write(registers.HL, H);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD (HL) L
            case 0b01110101:
                bus->write(registers.HL, L);
                ++cycleToExecute;
                ++registers.PC;
                break;
            //LD (HL) n
            case 0b00110110:
                ++registers.PC;
                bus->write(registers.HL, bus->read(registers.PC));
                cycleToExecute += 2;
                ++registers.PC;
            //LD A (BC)
            case 0b00001010:
                registers.reg_8[A] = bus->read(registers.BC);
                ++cycleToExecute;
                ++registers.PC;
            //LD A (DE)
            case 0b00011010:
                registers.reg_8[A] = bus->read(registers.DE);
                ++cycleToExecute;
                ++registers.PC;
            //LD A (C)
            case 0b11110010:
                registers.reg_8[A] = bus->read(0xFF00 | C);
                ++cycleToExecute;
                ++registers.PC;
            //LD (C) A
            case 0b11100010:
                bus->write(0xFF00 | C, A);
                ++cycleToExecute;
                ++registers.PC;
            //LD A (n)
            case 0b11110000:
                ++registers.PC;
                registers.reg_8[A] = bus->read(0xFF00 | bus->read(registers.PC));
                cycleToExecute += 2;
                ++registers.PC;
            //LD (n) A
            case 0b11100000:
                ++registers.PC;
                bus->write(0xFF00 | bus->read(registers.PC), A);
                cycleToExecute += 2;
                ++registers.PC;
            //LD A (nn)
            case 0b11111010:
                ++registers.PC;
                registers.reg_8[A] = bus->read(bus->read(registers.PC << 8 | bus->read(registers.PC + 1)));
                registers.PC += 2;
                cycleToExecute += 4;
            //LD (nn) A
            case 0b11101010:
                ++registers.PC;
                bus->write(bus->read(registers.PC << 8 | bus->read(registers.PC + 1)), A);
                registers.PC += 2;
                cycleToExecute += 4;
            //LD A (HLI)
            case 0b00101010:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = bus->read(helpVariable16);
                ++helpVariable16;
                registers.HL = helpVariable16;
                ++registers.PC;
                ++cycleToExecute;
            //LD A (HLD)
            case 0b00111010:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = bus->read(helpVariable16);
                --helpVariable16;
                registers.HL = helpVariable16;
                ++registers.PC;
                ++cycleToExecute;
            //LD (BC) A
            case 0b00000010:
                bus->write(registers.BC, A);
                ++cycleToExecute;
                ++registers.PC;
            //LD (DE) A
            case 0b00010010:
                bus->write(registers.DE, A);
                ++cycleToExecute;
                ++registers.PC;
            //LD (HLI) A
            case 0b00100010:
                helpVariable16 = registers.HL;
                bus->write(helpVariable16, A);
                ++helpVariable16;
                registers.HL = helpVariable16;
                ++registers.PC;
                ++cycleToExecute;
            case 0b00110010:
                //LD (HLD) A
                helpVariable16 = registers.HL;
                bus->write(helpVariable16, A);
                --helpVariable16;
                registers.HL = helpVariable16;
                ++registers.PC;
                ++cycleToExecute;
#pragma endregion 8_BIT_TRANSFER
#pragma region 16_BIT_TRANSFER
            // LM BC nn
            case 0b00000001:
                ++registers.PC;
                registers.reg_8[B] = bus->read(bus->read(registers.PC));
                ++registers.PC;
                registers.reg_8[C] = bus->read(bus->read(registers.PC));
                ++registers.PC;
                cycleNumber += 2;
                break;
            // LM DE nn
            case 0b00010001:
                ++registers.PC;
                registers.reg_8[E] = bus->read(bus->read(registers.PC));
                ++registers.PC;
                registers.reg_8[D] = bus->read(bus->read(registers.PC));
                ++registers.PC;
                cycleNumber += 2;
                break;
            // LM HL nn
            case 0b00100001:
                ++registers.PC;
                registers.reg_8[H] = bus->read(bus->read(registers.PC));
                ++registers.PC;
                registers.reg_8[L] = bus->read(bus->read(registers.PC));
                ++registers.PC;
                cycleNumber += 2;
                break;
            // LM registers.SP nn
            case 0b00110001:
                ++registers.PC;
                registers.SP = bus->read(bus->read(registers.PC)) | bus->read(bus->read(registers.PC + 1) << 8);
                registers.PC += 2;
                cycleNumber += 2;
                break;
            //LD registers.SP HL
            case 0b011111001:
                registers.SP = registers.HL;
                registers.PC += 2;
                ++cycleNumber;
                break;
            //PUSH BC
            case 0b11000101:
                --registers.SP;
                bus->write(registers.SP, B);
                --registers.SP;
                bus->write(registers.SP, C);
                ++registers.PC;
                cycleToExecute += 3;
                break;
            //PUSH DE
            case 0b11010101:
                --registers.SP;
                bus->write(registers.SP, D);
                --registers.SP;
                bus->write(registers.SP, E);
                ++registers.PC;
                cycleToExecute += 3;
                break;
            //PUSH HL
            case 0b11100101:
                --registers.SP;
                bus->write(registers.SP, H);
                --registers.SP;
                bus->write(registers.SP, L);
                ++registers.PC;
                cycleToExecute += 3;
                break;
            //PUSH AF
            case 0b11110101:
                --registers.SP;
                bus->write(registers.SP, A);
                --registers.SP;
                bus->write(registers.SP, registers.reg_8[F]);
                ++registers.PC;
                cycleToExecute += 3;
                break;
            //POP BC
            case 0b11000001:
                registers.reg_8[C] = bus->read(bus->read(registers.SP));
                ++registers.SP;
                registers.reg_8[B] = bus->read(bus->read(registers.SP));
                ++registers.SP;
                ++registers.PC;
                cycleToExecute += 2;
                break;
            //POP DE
            case 0b11010001:
                registers.reg_8[E] = bus->read(bus->read(registers.SP));
                ++registers.SP;
                registers.reg_8[D] = bus->read(bus->read(registers.SP));
                ++registers.SP;
                ++registers.PC;
                cycleToExecute += 2;
                break;
            //POP HL
            case 0b11100001:
                registers.reg_8[L] = bus->read(bus->read(registers.SP));
                ++registers.SP;
                registers.reg_8[H] = bus->read(bus->read(registers.SP));
                ++registers.SP;
                ++registers.PC;
                cycleToExecute += 2;
                break;
            //POP AF
            case 0b11110001:
                registers.reg_8[F] = bus->read(bus->read(registers.SP));
                ++registers.SP;
                registers.reg_8[A] = bus->read(bus->read(registers.SP));
                ++registers.SP;
                ++registers.PC;
                cycleToExecute += 2;
                break;
            //LDHL registers.SP e
            case 0b11111000:
                helpVariable8 = bus->read(bus->read(registers.PC));
                helpVariable16 = registers.SP + static_cast<int8_t>(helpVariable8);
                registers.reg_8[F] = (registers.SP > helpVariable16) << 4 | (
                                         (registers.SP & 0x0F) + (helpVariable8 & 0x0F) > 0x0F) << 5;
                registers.reg_8[H] = helpVariable16 >> 8;
                registers.reg_8[L] = helpVariable16 & 0xFF;
                cycleToExecute += 2;
            //LD (nn) registers.SP
            case 0b00001000:
                ++registers.PC;
                bus->write(bus->read(registers.PC), registers.reg_8[SP_LOW]);
                ++registers.PC;
                bus->write(bus->read(registers.PC), registers.reg_8[SP_HIGH]);
                ++registers.PC;
                cycleToExecute += 4;
#pragma endregion 16_BIT_TRANSFER
#pragma region 8_BIT_ARITHMETIC
            //ADD A A
            case 0b10000111:
                registers.reg_8[A] = arithmetic_8<ADD>(A, A, registers);
                ++registers.PC;
                break;
            //ADD A B
            case 0b10000000:
                registers.reg_8[A] = arithmetic_8<ADD>(A, B, registers);
                ++registers.PC;
                break;
            //ADD A C
            case 0b10000001:
                registers.reg_8[A] = arithmetic_8<ADD>(A, C, registers);
                ++registers.PC;
                break;
            //ADD A D
            case 0b10000010:
                registers.reg_8[A] = arithmetic_8<ADD>(A, D, registers);
                ++registers.PC;
                break;
            //ADD A E
            case 0b10000011:
                registers.reg_8[A] = arithmetic_8<ADD>(A, E, registers);
                ++registers.PC;
                break;
            //ADD A H
            case 0b10000100:
                registers.reg_8[A] = arithmetic_8<ADD>(A, H, registers);
                ++registers.PC;
                break;
            //ADD A L
            case 0b10000101:
                registers.reg_8[A] = arithmetic_8<ADD>(A, L, registers);
                ++registers.PC;
                break;
            //ADD A n
            case 0b11000110:
                ++registers.PC;
                registers.reg_8[A] = arithmetic_8<ADD>(A, bus->read(registers.PC), registers);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //ADD A (HL)
            case 0b10000110:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = arithmetic_8<ADD>(A, bus->read(helpVariable16), registers);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //ADC A A
            case 0b10001111:
                registers.reg_8[A] = arithmetic_8<ADC>(A, A, registers);
                ++registers.PC;
                break;
            //ADC A B
            case 0b10001000:
                registers.reg_8[A] = arithmetic_8<ADC>(A, B, registers);
                ++registers.PC;
                break;
            //ADC A C
            case 0b10001001:
                registers.reg_8[A] = arithmetic_8<ADC>(A, C, registers);
                ++registers.PC;
                break;
            //ADC A D
            case 0b10001010:
                registers.reg_8[A] = arithmetic_8<ADC>(A, D, registers);
                ++registers.PC;
                break;
            //ADC A E
            case 0b10001011:
                registers.reg_8[A] = arithmetic_8<ADC>(A, E, registers);
                ++registers.PC;
                break;
            //ADC A H
            case 0b10001100:
                registers.reg_8[A] = arithmetic_8<ADC>(A, H, registers);
                ++registers.PC;
                break;
            //ADC A L
            case 0b10001101:
                registers.reg_8[A] = arithmetic_8<ADC>(A, L, registers);
                ++registers.PC;
                break;
            //ADC A n
            case 0b11001110:
                ++registers.PC;
                registers.reg_8[A] = arithmetic_8<ADC>(A, bus->read(registers.PC), registers);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //ADC A (HL)
            case 0b10001110:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = arithmetic_8<ADC>(A, bus->read(helpVariable16), registers);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //SUB A A
            case 0b10010111:
                registers.reg_8[A] = arithmetic_8<SUB>(A, A, registers);
                ++registers.PC;
                break;
            //SUB A B
            case 0b10010000:
                registers.reg_8[A] = arithmetic_8<SUB>(A, B, registers);
                ++registers.PC;
                break;
            //SUB A C
            case 0b10010001:
                registers.reg_8[A] = arithmetic_8<SUB>(A, C, registers);
                ++registers.PC;
                break;
            //SUB A D
            case 0b10010010:
                registers.reg_8[A] = arithmetic_8<SUB>(A, D, registers);
                ++registers.PC;
                break;
            //SUB A E
            case 0b10010011:
                registers.reg_8[A] = arithmetic_8<SUB>(A, E, registers);
                ++registers.PC;
                break;
            //SUB A H
            case 0b10010100:
                registers.reg_8[A] = arithmetic_8<SUB>(A, H, registers);
                ++registers.PC;
                break;
            //SUB A L
            case 0b10010101:
                registers.reg_8[A] = arithmetic_8<SUB>(A, L, registers);
                ++registers.PC;
                break;
            //SUB A n
            case 0b11010110:
                ++registers.PC;
                registers.reg_8[A] = arithmetic_8<SUB>(A, bus->read(registers.PC), registers);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //SUB A (HL)
            case 0b10010110:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = arithmetic_8<SUB>(A, bus->read(helpVariable16), registers);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //SBC A A
            case 0b10011111:
                registers.reg_8[A] = arithmetic_8<SBC>(A, A, registers);
                ++registers.PC;
                break;
            //SBC A B
            case 0b10011000:
                registers.reg_8[A] = arithmetic_8<SBC>(A, B, registers);
                ++registers.PC;
                break;
            //SBC A C
            case 0b10011001:
                registers.reg_8[A] = arithmetic_8<SBC>(A, C, registers);
                ++registers.PC;
                break;
            //SBC A D
            case 0b10011010:
                registers.reg_8[A] = arithmetic_8<SBC>(A, D, registers);
                ++registers.PC;
                break;
            //SBC A E
            case 0b10011011:
                registers.reg_8[A] = arithmetic_8<SBC>(A, E, registers);
                ++registers.PC;
                break;
            //SBC A H
            case 0b10011100:
                registers.reg_8[A] = arithmetic_8<SBC>(A, H, registers);
                ++registers.PC;
                break;
            //SBC A L
            case 0b10011101:
                registers.reg_8[A] = arithmetic_8<SBC>(A, L, registers);
                ++registers.PC;
                break;
            //SBC A n
            case 0b11011110:
                ++registers.PC;
                registers.reg_8[A] = arithmetic_8<SBC>(A, bus->read(registers.PC), registers);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //SBC A (HL)
            case 0b10011110:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = arithmetic_8<SUB>(A, bus->read(helpVariable16), registers);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //AND A A
            case 0b10100111:
                registers.reg_8[A] = arithmetic_8<AND>(A, A, registers);
                ++registers.PC;
                break;
            //AND A B
            case 0b10100000:
                registers.reg_8[A] = arithmetic_8<AND>(A, B, registers);
                ++registers.PC;
                break;
            //AND A C
            case 0b10100001:
                registers.reg_8[A] = arithmetic_8<AND>(A, C, registers);
                ++registers.PC;
                break;
            //AND A D
            case 0b10100010:
                registers.reg_8[A] = arithmetic_8<AND>(A, D, registers);
                ++registers.PC;
                break;
            //AND A E
            case 0b10100011:
                registers.reg_8[A] = arithmetic_8<AND>(A, E, registers);
                ++registers.PC;
                break;
            //AND A H
            case 0b10100100:
                registers.reg_8[A] = arithmetic_8<AND>(A, H, registers);
                ++registers.PC;
                break;
            //AND A L
            case 0b10100101:
                registers.reg_8[A] = arithmetic_8<AND>(A, L, registers);
                ++registers.PC;
                break;
            //AND A n
            case 0b11100110:
                ++registers.PC;
                registers.reg_8[A] = arithmetic_8<AND>(A, bus->read(registers.PC), registers);
                registers.reg_8[A] &= bus->read(registers.PC);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //AND A (HL)
            case 0b10100110:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = arithmetic_8<AND>(A, bus->read(helpVariable16), registers);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //OR A A
            case 0b10110111:
                registers.reg_8[A] = arithmetic_8<OR>(A, A, registers);
                ++registers.PC;
                break;
            //OR A B
            case 0b10110000:
                registers.reg_8[A] = arithmetic_8<OR>(A, B, registers);
                ++registers.PC;
                break;
            //OR A C
            case 0b10110001:
                registers.reg_8[A] = arithmetic_8<OR>(A, C, registers);
                ++registers.PC;
                break;
            //OR A D
            case 0b10110010:
                registers.reg_8[A] = arithmetic_8<OR>(A, D, registers);
                ++registers.PC;
                break;
            //OR A E
            case 0b10110011:
                registers.reg_8[A] = arithmetic_8<OR>(A, E, registers);
                ++registers.PC;
                break;
            //OR A H
            case 0b10110100:
                registers.reg_8[A] = arithmetic_8<OR>(A, H, registers);
                ++registers.PC;
                break;
            //OR A L
            case 0b10110101:
                registers.reg_8[A] = arithmetic_8<OR>(A, L, registers);
                ++registers.PC;
                break;
            //OR A n
            case 0b11110110:
                ++registers.PC;
                registers.reg_8[A] = arithmetic_8<OR>(A, bus->read(registers.PC), registers);
                registers.reg_8[A] &= bus->read(registers.PC);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //OR A (HL)
            case 0b10110110:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = arithmetic_8<OR>(A, bus->read(helpVariable16), registers);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //XOR A A
            case 0b10101111:
                registers.reg_8[A] = arithmetic_8<XOR>(A, A, registers);
                ++registers.PC;
                break;
            //XOR A B
            case 0b10101000:
                registers.reg_8[A] = arithmetic_8<XOR>(A, B, registers);
                ++registers.PC;
                break;
            //XOR A C
            case 0b10101001:
                registers.reg_8[A] = arithmetic_8<XOR>(A, C, registers);
                ++registers.PC;
                break;
            //XOR A D
            case 0b10101010:
                registers.reg_8[A] = arithmetic_8<XOR>(A, D, registers);
                ++registers.PC;
                break;
            //XOR A E
            case 0b10101011:
                registers.reg_8[A] = arithmetic_8<XOR>(A, E, registers);
                ++registers.PC;
                break;
            //XOR A H
            case 0b10101100:
                registers.reg_8[A] = arithmetic_8<XOR>(A, H, registers);
                ++registers.PC;
                break;
            //XOR A L
            case 0b10101101:
                registers.reg_8[A] = arithmetic_8<XOR>(A, L, registers);
                ++registers.PC;
                break;
            //XOR A n
            case 0b11101110:
                ++registers.PC;
                registers.reg_8[A] = arithmetic_8<XOR>(A, bus->read(registers.PC), registers);
                registers.reg_8[A] &= bus->read(registers.PC);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //XOR A (HL)
            case 0b10101110:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = arithmetic_8<XOR>(A, bus->read(helpVariable16), registers);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //CP A A
            case 0b10111111:
                registers.reg_8[A] = arithmetic_8<CP>(A, A, registers);
                ++registers.PC;
                break;
            //CP A B
            case 0b10111000:
                registers.reg_8[A] = arithmetic_8<CP>(A, B, registers);
                ++registers.PC;
                break;
            //CP A C
            case 0b10111001:
                registers.reg_8[A] = arithmetic_8<CP>(A, C, registers);
                ++registers.PC;
                break;
            //CP A D
            case 0b10111010:
                registers.reg_8[A] = arithmetic_8<CP>(A, D, registers);
                ++registers.PC;
                break;
            //CP A E
            case 0b10111011:
                registers.reg_8[A] = arithmetic_8<CP>(A, E, registers);
                ++registers.PC;
                break;
            //CP A H
            case 0b10111100:
                registers.reg_8[A] = arithmetic_8<CP>(A, H, registers);
                ++registers.PC;
                break;
            //CP A L
            case 0b10111101:
                registers.reg_8[A] = arithmetic_8<CP>(A, L, registers);
                ++registers.PC;
                break;
            //CP A n
            case 0b11111110:
                ++registers.PC;
                registers.reg_8[A] = arithmetic_8<CP>(A, bus->read(registers.PC), registers);
                registers.reg_8[A] &= bus->read(registers.PC);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //CP A (HL)
            case 0b10111110:
                helpVariable16 = registers.HL;
                registers.reg_8[A] = arithmetic_8<CP>(A, bus->read(helpVariable16), registers);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //INC A
            case 0b00111100:
                registers.reg_8[A] = arithmetic_8<INC>(A, 1, registers);
                ++registers.PC;
                break;
            //INC B
            case 0b00000100:
                registers.reg_8[B] = arithmetic_8<INC>(B, 1, registers);
                ++registers.PC;
                break;
            //INC C
            case 0b00001100:
                registers.reg_8[C] = arithmetic_8<INC>(C, 1, registers);
                ++registers.PC;
                break;
            //INC D
            case 0b00010100:
                registers.reg_8[D] = arithmetic_8<INC>(D, 1, registers);
                ++registers.PC;
                break;
            //INC E
            case 0b00011100:
                registers.reg_8[E] = arithmetic_8<INC>(E, 1, registers);
                ++registers.PC;
                break;
            //INC H
            case 0b00100100:
                registers.reg_8[H] = arithmetic_8<INC>(H, 1, registers);
                ++registers.PC;
                break;
            //INC L
            case 0b00101100:
                registers.reg_8[L] = arithmetic_8<INC>(L, 1, registers);
                ++registers.PC;
                break;
            //INC (HL)
            case 0b00110100:
                helpVariable16 = registers.HL;
                bus->write(helpVariable16, arithmetic_8<INC>(bus->read(helpVariable16), 1, registers));
                cycleToExecute += 2;
                ++registers.PC;
                break;
            //DEC A
            case 0b00111101:
                registers.reg_8[A] = arithmetic_8<DEC>(A, 1, registers);
                ++registers.PC;
                break;
            //DEC B
            case 0b00000101:
                registers.reg_8[B] = arithmetic_8<DEC>(B, 1, registers);
                ++registers.PC;
                break;
            //DEC C
            case 0b00001101:
                registers.reg_8[C] = arithmetic_8<DEC>(C, 1, registers);
                ++registers.PC;
                break;
            //DEC D
            case 0b00010101:
                registers.reg_8[D] = arithmetic_8<DEC>(D, 1, registers);
                ++registers.PC;
                break;
            //DEC E
            case 0b00011101:
                registers.reg_8[E] = arithmetic_8<DEC>(E, 1, registers);
                ++registers.PC;
                break;
            //DEC H
            case 0b00100101:
                registers.reg_8[H] = arithmetic_8<DEC>(H, 1, registers);
                ++registers.PC;
                break;
            //DEC L
            case 0b00101101:
                registers.reg_8[L] = arithmetic_8<DEC>(L, 1, registers);
                ++registers.PC;
                break;
            //DEC (HL)
            case 0b00110101:
                helpVariable16 = registers.HL;
                bus->write(helpVariable16, arithmetic_8<DEC>(bus->read(helpVariable16), 1, registers));
                cycleToExecute += 2;
                ++registers.PC;
                break;


#pragma endregion 8_BIT_ARITHMETIC
#pragma region 16_BIT_ARITHMETIC
            // ADD HL BC
            case 0b00001001:
                helpVariable16 = arithmetic_16<ADD>(registers.HL, registers.BC, registers);
                registers.reg_8[H] = helpVariable16 >> 8;
                registers.reg_8[L] = helpVariable16 & 0xFF;
                ++registers.PC;
                ++cycleToExecute;
                break;
            // ADD HL DE
            case 0b00011001:
                helpVariable16 = arithmetic_16<ADD>(registers.HL, registers.DE, registers);
                registers.reg_8[H] = helpVariable16 >> 8;
                registers.reg_8[L] = helpVariable16 & 0xFF;
                ++registers.PC;
                ++cycleToExecute;
                break;
            // ADD HL HL
            case 0b00101001:
                helpVariable16 = registers.HL;
                helpVariable16 = arithmetic_16<ADD>(helpVariable16, helpVariable16, registers);
                registers.reg_8[H] = helpVariable16 >> 8;
                registers.reg_8[L] = helpVariable16 & 0xFF;
                ++registers.PC;
                ++cycleToExecute;
                break;
            // ADD HL registers.SP
            case 0b00111001:
                helpVariable16 = arithmetic_16<ADD>(registers.HL, registers.SP, registers);
                registers.reg_8[H] = helpVariable16 >> 8;
                registers.reg_8[L] = helpVariable16 & 0xFF;
                ++registers.PC;
                ++cycleToExecute;
                break;
            //ADD registers.SP e
            case 0b11101000:
                ++registers.PC;
                registers.reg_8[F] &= 0b00111111;
                registers.SP = arithmetic_16<ADD>(registers.SP, bus->read(registers.PC), registers);
                ++registers.PC;
                cycleToExecute += 3;
                break;
            //INC BC
            case 0b00000011:
                helpVariable16 = arithmetic_16<INC>(registers.BC, 1, registers);
                registers.reg_8[B] = helpVariable16 >> 8;
                registers.reg_8[C] = helpVariable16 & 0xFF;
                ++registers.PC;
                ++cycleToExecute;
                break;
            //INC DE
            case 0b00010011:
                helpVariable16 = arithmetic_16<INC>(registers.DE, 1, registers);
                registers.reg_8[D] = helpVariable16 >> 8;
                registers.reg_8[E] = helpVariable16 & 0xFF;
                ++registers.PC;
                ++cycleToExecute;
                break;
            //INC HL
            case 0b00100011:
                helpVariable16 = arithmetic_16<INC>(registers.HL, 1, registers);
                registers.reg_8[H] = helpVariable16 >> 8;
                registers.reg_8[L] = helpVariable16 & 0xFF;
                ++registers.PC;
                ++cycleToExecute;
                break;
            //INC registers.SP
            case 0b00110011:
                registers.SP = arithmetic_16<INC>(registers.SP, 1, registers);
                ++registers.PC;
                ++cycleToExecute;
                break;
            //DEC BC
            case 0b00001011:
                helpVariable16 = arithmetic_16<DEC>(registers.BC, 1, registers);
                registers.reg_8[B] = helpVariable16 >> 8;
                registers.reg_8[C] = helpVariable16 & 0xFF;
                ++registers.PC;
                ++cycleToExecute;
                break;
            //DEC DE
            case 0b00011011:
                helpVariable16 = arithmetic_16<DEC>(registers.DE, 1, registers);
                registers.reg_8[D] = helpVariable16 >> 8;
                registers.reg_8[E] = helpVariable16 & 0xFF;
                ++registers.PC;
                ++cycleToExecute;
                break;
            //DEC HL
            case 0b00101011:
                helpVariable16 = arithmetic_16<DEC>(registers.HL, 1, registers);
                registers.reg_8[H] = helpVariable16 >> 8;
                registers.reg_8[L] = helpVariable16 & 0xFF;
                ++registers.PC;
                ++cycleToExecute;
                break;
            //DEC registers.SP
            case 0b00111011:
                registers.SP = arithmetic_16<DEC>(registers.SP, 1, registers);
                ++registers.PC;
                ++cycleToExecute;
                break;


#pragma endregion 16_BIT_ARITHMETIC
#pragma region ROTATION_SHIFT
            //RLCA
            case 0b00000111:
                registers.reg_8[A] = shift_rotation<RLC>(A, registers);
                ++registers.PC;
                break;
            //RLA
            case 0b00010111:
                registers.reg_8[A] = shift_rotation<RL>(A, registers);
                ++registers.PC;
                break;
            //RRCA
            case 0b00001111:
                registers.reg_8[A] = shift_rotation<RRC>(A, registers);
                ++registers.PC;
                break;
            //RRA
            case 0b00011111:
                registers.reg_8[A] = shift_rotation<RR>(A, registers);
                ++registers.PC;
                break;
            // double line instruction
            case 0b11001011:
                ++registers.PC;
                switch (bus->read(registers.PC)) {
                    //RLC A
                    case 0b00000111:
                        registers.reg_8[A] = shift_rotation<RLC>(A, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RLC B
                    case 0b00000000:
                        registers.reg_8[B] = shift_rotation<RLC>(B, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RLC C
                    case 0b00000001:
                        registers.reg_8[C] = shift_rotation<RLC>(C, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RLC D
                    case 0b00000010:
                        registers.reg_8[D] = shift_rotation<RLC>(D, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RLC E
                    case 0b00000011:
                        registers.reg_8[E] = shift_rotation<RLC>(E, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RLC H
                    case 0b00000100:
                        registers.reg_8[H] = shift_rotation<RLC>(H, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RLC L
                    case 0b00000101:
                        registers.reg_8[L] = shift_rotation<RLC>(L, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RLC (HL)
                    case 0b00000110:
                        helpVariable16 = registers.HL;
                        bus->write(shift_rotation<RLC>(bus->read(helpVariable16), registers), helpVariable16);
                        ++registers.PC;
                        cycleToExecute += 3;
                        break;
                    //RL A
                    case 0b00010111:
                        registers.reg_8[A] = shift_rotation<RL>(A, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RL B
                    case 0b00010000:
                        registers.reg_8[B] = shift_rotation<RL>(B, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RL C
                    case 0b00010001:
                        registers.reg_8[C] = shift_rotation<RL>(C, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RL D
                    case 0b00010010:
                        registers.reg_8[D] = shift_rotation<RL>(D, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RL E
                    case 0b00010011:
                        registers.reg_8[E] = shift_rotation<RL>(E, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RL H
                    case 0b00010100:
                        registers.reg_8[H] = shift_rotation<RL>(H, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RL L
                    case 0b00010101:
                        registers.reg_8[L] = shift_rotation<RL>(L, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RL (HL)
                    case 0b00010110:
                        helpVariable16 = registers.HL;
                        bus->write(shift_rotation<RL>(bus->read(helpVariable16), registers), helpVariable16);
                        ++registers.PC;
                        cycleToExecute += 3;
                        break;
                    //RRC A
                    case 0b00001111:
                        registers.reg_8[A] = shift_rotation<RRC>(A, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RRC B
                    case 0b00001000:
                        registers.reg_8[B] = shift_rotation<RRC>(B, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RRC C
                    case 0b00001001:
                        registers.reg_8[C] = shift_rotation<RRC>(C, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RRC D
                    case 0b00001010:
                        registers.reg_8[D] = shift_rotation<RRC>(D, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RRC E
                    case 0b00001011:
                        registers.reg_8[E] = shift_rotation<RRC>(E, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RRC H
                    case 0b00001100:
                        registers.reg_8[H] = shift_rotation<RRC>(H, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RRC L
                    case 0b00001101:
                        registers.reg_8[L] = shift_rotation<RRC>(L, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RRC (HL)
                    case 0b00001110:
                        helpVariable16 = registers.HL;
                        bus->write(shift_rotation<RRC>(bus->read(helpVariable16), registers), helpVariable16);
                        ++registers.PC;
                        cycleToExecute += 3;
                        break;
                    //RR A
                    case 0b00011111:
                        registers.reg_8[A] = shift_rotation<RR>(A, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RR B
                    case 0b00011000:
                        registers.reg_8[B] = shift_rotation<RR>(B, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RR C
                    case 0b00011001:
                        registers.reg_8[C] = shift_rotation<RR>(C, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RR D
                    case 0b00011010:
                        registers.reg_8[D] = shift_rotation<RR>(D, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RR E
                    case 0b00011011:
                        registers.reg_8[E] = shift_rotation<RR>(E, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RR H
                    case 0b00011100:
                        registers.reg_8[H] = shift_rotation<RR>(H, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RR L
                    case 0b00011101:
                        registers.reg_8[L] = shift_rotation<RR>(L, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //RR (HL)
                    case 0b00011110:
                        helpVariable16 = registers.HL;
                        bus->write(shift_rotation<RR>(bus->read(helpVariable16), registers), helpVariable16);
                        ++registers.PC;
                        cycleToExecute += 3;
                        break;
                    //SLA A
                    case 0b00100111:
                        registers.reg_8[A] = shift_rotation<SLA>(A, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SLA B
                    case 0b00100000:
                        registers.reg_8[B] = shift_rotation<SLA>(B, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SLA C
                    case 0b00100001:
                        registers.reg_8[C] = shift_rotation<SLA>(C, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SLA D
                    case 0b00100010:
                        registers.reg_8[D] = shift_rotation<SLA>(D, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SLA E
                    case 0b00100011:
                        registers.reg_8[E] = shift_rotation<SLA>(E, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SLA H
                    case 0b00100100:
                        registers.reg_8[H] = shift_rotation<SLA>(H, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SLA L
                    case 0b00100101:
                        registers.reg_8[L] = shift_rotation<SLA>(L, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SLA (HL)
                    case 0b00100110:
                        helpVariable16 = registers.HL;
                        bus->write(shift_rotation<SLA>(bus->read(helpVariable16), registers), helpVariable16);
                        ++registers.PC;
                        cycleToExecute += 3;
                        break;
                    //SRA A
                    case 0b00101111:
                        registers.reg_8[A] = shift_rotation<SRA>(A, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SRA B
                    case 0b00101000:
                        registers.reg_8[B] = shift_rotation<SRA>(B, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SRA C
                    case 0b00101001:
                        registers.reg_8[C] = shift_rotation<SRA>(C, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SRA D
                    case 0b00101010:
                        registers.reg_8[D] = shift_rotation<SRA>(D, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SRA E
                    case 0b00101011:
                        registers.reg_8[E] = shift_rotation<SRA>(E, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SRA H
                    case 0b00101100:
                        registers.reg_8[H] = shift_rotation<SRA>(H, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SRA L
                    case 0b00101101:
                        registers.reg_8[L] = shift_rotation<SRA>(L, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SRA (HL)
                    case 0b00101110:
                        helpVariable16 = registers.HL;
                        bus->write(shift_rotation<SRA>(bus->read(helpVariable16), registers), helpVariable16);
                        ++registers.PC;
                        cycleToExecute += 3;
                        break;
                    //SRL A
                    case 0b00111111:
                        registers.reg_8[A] = shift_rotation<SRL>(A, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SRL B
                    case 0b00111000:
                        registers.reg_8[B] = shift_rotation<SRL>(B, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SRL C
                    case 0b00111001:
                        registers.reg_8[C] = shift_rotation<SRL>(C, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SRL D
                    case 0b00111010:
                        registers.reg_8[D] = shift_rotation<SRL>(D, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SRL E
                    case 0b00111011:
                        registers.reg_8[E] = shift_rotation<SRL>(E, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SRL H
                    case 0b00111100:
                        registers.reg_8[H] = shift_rotation<SRL>(H, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SRL L
                    case 0b00111101:
                        registers.reg_8[L] = shift_rotation<SRL>(L, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SRL (HL)
                    case 0b00111110:
                        helpVariable16 = registers.HL;
                        bus->write(shift_rotation<SRL>(bus->read(helpVariable16), registers), helpVariable16);
                        ++registers.PC;
                        cycleToExecute += 3;
                        break;
                    //SWAP A
                    case 0b00110111:
                        registers.reg_8[A] = shift_rotation<SRL>(A, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SWAP B
                    case 0b00110000:
                        registers.reg_8[B] = shift_rotation<SRL>(B, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SWAP C
                    case 0b00110001:
                        registers.reg_8[C] = shift_rotation<SRL>(C, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SWAP D
                    case 0b00110010:
                        registers.reg_8[D] = shift_rotation<SRL>(D, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SWAP E
                    case 0b00110011:
                        registers.reg_8[E] = shift_rotation<SRL>(E, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SWAP H
                    case 0b00110100:
                        registers.reg_8[H] = shift_rotation<SRL>(H, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SWAP L
                    case 0b00110101:
                        registers.reg_8[L] = shift_rotation<SRL>(L, registers);
                        ++registers.PC;
                        ++cycleToExecute;
                        break;
                    //SWAP (HL)
                    case 0b00110110:
                        helpVariable16 = registers.HL;
                        bus->write(shift_rotation<SRL>(bus->read(helpVariable16), registers), helpVariable16);
                        ++registers.PC;
                        cycleToExecute += 3;
                        break;
#pragma endregion ROTATION_SHIFT
#pragma region BIT_OPERATORS
                    default:
                        switch (instruction >> 6) {
                            //BIT
                            case 0b01:
                                if (instruction & 0b111 == 0b110) {
                                    bit_operation<BIT>((instruction & 0b111000) >> 3, bus->read(registers.HL),
                                                       registers);
                                    cycleToExecute += 2;
                                } else {
                                    bit_operation<BIT>((instruction & 0b111000) >> 3,
                                                       registers.reg_8[instruction & 0b111], registers);
                                    ++cycleToExecute;
                                }
                                ++registers.PC;
                                break;
                            //SET
                            case 0b11:
                                if (instruction & 0b111 == 0b110) {
                                    bus->write(registers.HL,
                                               bit_operation<SET>((instruction & 0b111000) >> 3,
                                                                  bus->read(registers.HL), registers));
                                    cycleToExecute += 3;
                                } else {
                                    registers.reg_8[instruction & 0b111] = bit_operation<SET>(
                                        (instruction & 0b111000) >> 3, registers.reg_8[instruction & 0b111], registers);
                                    ++cycleToExecute;
                                }
                                ++registers.PC;
                                break;
                            //RES
                            case 0b10:
                                if (instruction & 0b111 == 0b110) {
                                    bus->write(registers.HL,
                                               bit_operation<RES>((instruction & 0b111000) >> 3,
                                                                  bus->read(registers.HL), registers));
                                    cycleToExecute += 3;
                                } else {
                                    registers.reg_8[instruction & 0b111] = bit_operation<RES>(
                                        (instruction & 0b111000) >> 3, registers.reg_8[instruction & 0b111], registers);
                                    ++cycleToExecute;
                                }
                                ++registers.PC;
                                break;
                        }

                    // BIT b A
                }
#pragma endregion BIT_OPERATORS
#pragma region JUMP
            // JP nn
            case 0b11000011:
                registers.PC = bus->read(++registers.PC) << 8 | bus->read(++registers.PC);
                cycleNumber += 3;
                break;
            // JP NZ nn
            case 0b11000010:
                if (!registers.z) {
                    helpVariable16 = registers.PC;
                    registers.reg_8[PC_LOW] = bus->read(++helpVariable16);
                    registers.reg_8[PC_HIGH] = bus->read(++helpVariable16);
                    cycleToExecute += 4;
                } else {
                    registers.PC += 3;
                    cycleToExecute += 2;
                }
                break;
            // JP Z nn
            case 0b11001010:
                if (registers.z) {
                    helpVariable16 = registers.PC;
                    registers.reg_8[PC_LOW] = bus->read(++helpVariable16);
                    registers.reg_8[PC_HIGH] = bus->read(++helpVariable16);
                    registers.PC = helpVariable16;
                    cycleToExecute += 4;
                } else {
                    registers.PC += 3;
                    cycleToExecute += 2;
                }
                break;
            // JP NC nn
            case 0b11010010:
                if (!registers.cy) {
                    helpVariable16 = registers.PC;
                    registers.reg_8[PC_LOW] = bus->read(++helpVariable16);
                    registers.reg_8[PC_HIGH] = bus->read(++helpVariable16);
                    registers.PC = helpVariable16;
                    cycleToExecute += 4;
                } else {
                    registers.PC += 3;
                    cycleToExecute += 2;
                }
                break;
            // JP C nn
            case 0b11011010:
                if (registers.cy) {
                    helpVariable16 = registers.PC;
                    registers.reg_8[PC_LOW] = bus->read(++helpVariable16);
                    registers.reg_8[PC_HIGH] = bus->read(++helpVariable16);
                    cycleToExecute += 4;
                } else {
                    registers.PC += 3;
                    cycleToExecute += 2;
                }
                break;
            // JR e
            case 0b00011000:
                registers.PC += static_cast<int8_t>(bus->read(++registers.PC));
                cycleNumber += 2;
                break;
            // JR NZ e
            case 0b00100000:
                helpVariable16 = registers.PC + static_cast<int8_t>(bus->read(++registers.PC));
                if (!registers.z) {
                    registers.PC = helpVariable16;
                    cycleToExecute += 2;
                } else {
                    ++registers.PC;
                    cycleToExecute += 1;
                }
                break;
            // JR Z 3
            case 0b00101000:
                helpVariable16 = registers.PC + static_cast<int8_t>(bus->read(++registers.PC));
                if (registers.z) {
                    registers.PC = helpVariable16;
                    cycleToExecute += 2;
                } else {
                    ++registers.PC;
                    cycleToExecute += 1;
                }
                break;
            // JR NC e
            case 0b00110000:
                helpVariable16 = registers.PC + static_cast<int8_t>(bus->read(++registers.PC));
                if (!registers.cy) {
                    registers.PC = helpVariable16;
                    cycleToExecute += 2;
                } else {
                    ++registers.PC;
                    cycleToExecute += 1;
                }
                break;
            // JR C 3
            case 0b00111000:
                helpVariable16 = registers.PC + static_cast<int8_t>(bus->read(++registers.PC));
                if (registers.cy) {
                    registers.PC = helpVariable16;
                    cycleToExecute += 2;
                } else {
                    ++registers.PC;
                    cycleToExecute += 1;
                }
                break;
            // JP (HL)
            case 0b11101001:
                registers.PC = registers.HL;
                break;
#pragma endregion JUMP
#pragma region CALL_RETURN
            // CALL nn
            case 0b11001101:
                bus->write(--registers.SP, registers.reg_8[PC_HIGH]);
                bus->write(--registers.SP, registers.reg_8[PC_LOW]);
                helpVariable16 = registers.PC;
                registers.reg_8[PC_LOW] = bus->read(++helpVariable16);
                registers.reg_8[PC_HIGH] = bus->read(++helpVariable16);
                cycleToExecute += 5;
                break;
            // CALL NZ nn
            case 0b11000100:
                if (!registers.z) {
                    bus->write(--registers.SP, registers.reg_8[PC_HIGH]);
                    bus->write(--registers.SP, registers.reg_8[PC_LOW]);
                    helpVariable16 = registers.PC;
                    registers.reg_8[PC_LOW] = bus->read(++helpVariable16);
                    registers.reg_8[PC_HIGH] = bus->read(++helpVariable16);
                    cycleToExecute += 5;
                } else {
                    registers.PC += 3;
                    cycleToExecute += 2;
                }
                break;
            // CALL Z nn
            case 0b11001100:
                if (registers.z) {
                    bus->write(--registers.SP, registers.reg_8[PC_HIGH]);
                    bus->write(--registers.SP, registers.reg_8[PC_LOW]);
                    helpVariable16 = registers.PC;
                    registers.reg_8[PC_LOW] = bus->read(++helpVariable16);
                    registers.reg_8[PC_HIGH] = bus->read(++helpVariable16);
                    cycleToExecute += 5;
                } else {
                    registers.PC += 3;
                    cycleToExecute += 2;
                }
                break;
            // CALL NC nn
            case 0b11010100:
                if (!registers.cy) {
                    bus->write(--registers.SP, registers.reg_8[PC_HIGH]);
                    bus->write(--registers.SP, registers.reg_8[PC_LOW]);
                    helpVariable16 = registers.PC;
                    registers.reg_8[PC_LOW] = bus->read(++helpVariable16);
                    registers.reg_8[PC_HIGH] = bus->read(++helpVariable16);
                    cycleToExecute += 5;
                } else {
                    registers.PC += 3;
                    cycleToExecute += 2;
                }
                break;
            // CALL C nn
            case 0b11011100:
                if (registers.cy) {
                    bus->write(--registers.SP, registers.reg_8[PC_HIGH]);
                    bus->write(--registers.SP, registers.reg_8[PC_LOW]);
                    helpVariable16 = registers.PC;
                    registers.reg_8[PC_LOW] = bus->read(++helpVariable16);
                    registers.reg_8[PC_HIGH] = bus->read(++helpVariable16);
                    cycleToExecute += 5;
                } else {
                    registers.PC += 3;
                    cycleToExecute += 2;
                }
                break;
            // RET
            case 0b11001001:
                registers.reg_8[PC_LOW] = bus->read(registers.SP++);
                registers.reg_8[PC_HIGH] = bus->read(registers.SP++);
                cycleToExecute += 3;
                break;
            // RETI to-do interupt
            case 0b11011001:
                registers.reg_8[PC_LOW] = bus->read(registers.SP++);
                registers.reg_8[PC_HIGH] = bus->read(registers.SP++);
                cycleToExecute += 3;
                break;
            // RET NZ nn
            case 0b11000000:
                if (!registers.z) {
                    registers.reg_8[PC_LOW] = bus->read(registers.SP++);
                    registers.reg_8[PC_HIGH] = bus->read(registers.SP++);
                    cycleToExecute += 4;
                } else {
                    registers.PC += 3;
                    cycleToExecute += 2;
                }
                break;
            // RET Z nn
            case 0b11001000:
                if (registers.z) {
                    registers.reg_8[PC_LOW] = bus->read(registers.SP++);
                    registers.reg_8[PC_HIGH] = bus->read(registers.SP++);
                    cycleToExecute += 4;
                } else {
                    ++registers.PC;
                    cycleToExecute += 1;
                }
                break;
            // RET NC nn
            case 0b11010000:
                if (!registers.cy) {
                    registers.reg_8[PC_LOW] = bus->read(registers.SP++);
                    registers.reg_8[PC_HIGH] = bus->read(registers.SP++);
                    cycleToExecute += 4;
                } else {
                    ++registers.PC;
                    cycleToExecute += 1;
                }
                break;
            // RET C nn
            case 0b11011000:
                if (registers.cy) {
                    registers.reg_8[PC_LOW] = bus->read(registers.SP++);
                    registers.reg_8[PC_HIGH] = bus->read(registers.SP++);
                    cycleToExecute += 4;
                } else {
                    ++registers.PC;
                    cycleToExecute += 1;
                }
                break;
            // RST t
            case 0b11000111:
            case 0b11001111:
            case 0b11010111:
            case 0b11011111:
            case 0b11100111:
            case 0b11101111:
            case 0b11110111:
            case 0b11111111:
                bus->write(--registers.SP, registers.reg_8[PC_HIGH]);
                bus->write(--registers.SP, registers.reg_8[PC_LOW]);
                registers.reg_8[PC_LOW] = instruction & 0b00111000;
                registers.reg_8[PC_HIGH] = 0x00;
                cycleToExecute += 3;
                break;
#pragma endregion CALL_RETURN
#pragma region GENERAL
            // DAA
            case 0b00100111:
                registers.h = false;
                helpVariable8 = 0;
                if (registers.n) {
                    if (!registers.cy && registers.h && registers.reg_8[A] <= 0x80 && registers.reg_8[A] & 0x0F >=
                        0x06) {
                        helpVariable8 = 0xFA;
                        registers.cy = false;
                    } else if (registers.cy && !registers.h && registers.reg_8[A] >= 0x70 && registers.reg_8[A] & 0x0F
                               <= 0x09) {
                        helpVariable8 = 0xA0;
                        registers.cy = true;
                    } else if (registers.cy && registers.h && registers.reg_8[A] >= 0x60 && registers.reg_8[A] & 0x0F >=
                               0x06) {
                        helpVariable8 = 0x9A;
                        registers.cy = true;
                    } else {
                        registers.cy = false;
                    }
                } else {
                    if (registers.cy || registers.reg_8[A] >= 0xA0) {
                        helpVariable8 = 0x60;
                    } else if (!registers.cy && !registers.h && registers.reg_8[A] >= 0x90 && registers.reg_8[A] & 0x0F
                               >= 0x0A) {
                        helpVariable8 = 0x60;
                        registers.cy = true;
                    }

                    if (registers.h || registers.reg_8[A] & 0x0F >= 0x0A) {
                        helpVariable8 += 0x06;
                        registers.cy = false;
                    }
                }
                registers.reg_8[A] += helpVariable8;
                registers.z = registers.reg_8[A] == 0;
                ++registers.PC;
                break;
            //NOP
            case 0b00000000:
                ++registers.PC;
                break;
            //CCF
            case 0b00111111:
                registers.cy = !registers.cy;
                ++registers.PC;
                break;
            //SCF
            case 0b00110111:
                registers.cy = true;
                ++registers.PC;
                break;
            //DI
            case 0b11110011:
                interruptEnabled = false;
                ++registers.PC;
                break;
            //EI
            case 0b11111011:
                interruptEnabled = true;
                ++registers.PC;
                break;
            //HALT
            case 0b01110110:
                haltMode = true;
                ++registers.PC;
                break;
            //STOP
            case 0b00010000:
                stopMode = true;
                ++registers.PC;
                break;
#pragma endregion GENERAL
            default:
                std::cout << "Invalid instruction :" << bus->read(registers.PC) << "\n";;
        }
    } else {
        --cycleToExecute;
    }
    cycleNumber++;
}

void CPU::Cpu::executeNCycles(const uint32_t n) {
    for (uint32_t i = 0; i < n; ++i) {
        executeCycle();
    }
}

void CPU::Cpu::run() {
    while (true) {
        executeCycle();
    }
}
