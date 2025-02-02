//
// Created by Jakub on 16.01.2025.
//

#include "Cpu.h"


CPU::Cpu::Cpu(BUS::Bus &bus): A(0), F(0), B(0), C(0), D(0), E(0), H(0), L(0), SP(0), PC(0) {
    this->bus = &bus;
    cycleNumber = 0;
    cycleToExecute = 0;
}


template<CPU::OPERATION_TYPE type>
constexpr uint8_t arithmetic_8(const uint8_t x, const uint8_t y, uint8_t &flag) {
    bool carry = (flag & 0b00010000) != 0;
    bool halfCarry = (flag & 0b00100000) != 0;
    bool subtraction = (flag & 0b01000000) != 0;
    bool zero = (flag & 0b10000000) != 0;
    uint16_t result = 0;
    if constexpr (type == CPU::OPERATION_TYPE::ADD) {
        result = x + y;
        halfCarry = (x & 0x0F) + (y & 0x0F) > 0x0F;
        subtraction = false;
        carry = result > 0xFF;
        zero = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::ADC) {
        result = x + y + carry;
        halfCarry = (x & 0x0F) + (y & 0x0F) + carry > 0x0F;
        subtraction = false;
        carry = result > 0xFF;
        zero = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SUB) {
        result = x - y;
        halfCarry = (x & 0x0F) < (y & 0x0F);
        subtraction = true;
        carry = x < y;
        zero = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SBC) {
        result = x - y - carry;
        halfCarry = (x & 0x0F) < (y & 0x0F) + carry;
        subtraction = true;
        carry = x < y + carry;
        zero = result == 0;
    }
    if constexpr (type == CPU::OPERATION_TYPE::AND) {
        result = x & y;
        halfCarry = true;
        subtraction = false;
        carry = false;
        zero = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::OR) {
        result = x | y;
        halfCarry = false;
        subtraction = false;
        carry = false;
        zero = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::XOR) {
        result = x ^ y;
        halfCarry = false;
        subtraction = false;
        carry = false;
        zero = result == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::CP) {
        result = x;
        halfCarry = (x & 0x0F) < (y & 0x0F);
        subtraction = true;
        carry = x < y;
        zero = x - y == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::INC) {
        result = x + 1;
        halfCarry = (x & 0x0F) + 1 > 0x0F;
        subtraction = false;
        carry = result > 0xFF;
        zero = x - y == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::DEC) {
        result = x - 1;
        halfCarry = (x & 0x0F) < 1;
        subtraction = true;
        carry = x < 1;
        zero = x - y == 0;
    }

    flag = zero << 7 | subtraction << 6 | halfCarry << 5 | (carry) << 4;

    return static_cast<uint8_t>(result);
}

template<CPU::OPERATION_TYPE type>
constexpr uint16_t arithmetic_16(const uint16_t x, const uint16_t y, uint8_t &flag) {
    bool carry = (flag & 0b00010000) != 0;
    bool halfCarry = (flag & 0b00100000) != 0;
    bool subtraction = (flag & 0b01000000) != 0;
    bool zero = (flag & 0b10000000) != 0;
    uint32_t result = 0;

    if constexpr (type == CPU::OPERATION_TYPE::ADD) {
        result = x + y;
        halfCarry = (x & 0x0FFF) + (y & 0x0FFF) > 0x0FFF;
        subtraction = false;
        carry = result > 0xFFFF;
    }

    if constexpr (type == CPU::OPERATION_TYPE::INC) {
        result = x + 1;
    }

    if constexpr (type == CPU::OPERATION_TYPE::DEC) {
        result = x - 1;
    }

    flag = zero << 7 | subtraction << 6 | halfCarry << 5 | (carry) << 4;

    return static_cast<uint16_t>(result);
}

template<CPU::OPERATION_TYPE type>
constexpr uint8_t shift_rotation(const uint8_t x, uint8_t &flag) {
    bool carry = (flag & 0b00010000) != 0;
    bool halfCarry = false;
    bool subtraction = false;
    bool zero = false;
    uint16_t result = 0;

    if constexpr (type == CPU::OPERATION_TYPE::RLC) {
        result = x << 1;
        carry = result > 0xFF;
        result = result + carry;
        zero = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::RL) {
        result = x << 1;
        result = result + carry;
        carry = result > 0xFF;
        zero = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::RRC) {
        carry = x & 1;
        result = x >> 1;
        result = result + carry << 7;
        zero = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::RR) {
        result = x + carry << 8;
        carry = x & 1;
        result = result >> 1;
        zero = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SLA) {
        result = x << 1;
        carry = result > 0xFF;
        zero = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SRA) {
        carry = x & 1;
        result = x >> 1 + x & 80;
        zero = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SRL) {
        carry = x & 1;
        result = x >> 1;
        zero = result && 0xFF == 0;
    }

    if constexpr (type == CPU::OPERATION_TYPE::SWAP) {
        result = x >> 4 + x << 4;
        zero = result && 0xFF == 0;
    }

    flag = zero << 7 | subtraction << 6 | halfCarry << 5 | (carry) << 4;

    return static_cast<uint8_t>(result);
}

void CPU::Cpu::executeCycle() {
    std::cout << "Execute Cycle:" << cycleNumber << "\n";
    uint16_t helpVariable16;
    uint8_t helpVariable8;
    uint8_t instruction;
    if (!cycleToExecute) {
        instruction = bus->read(PC);
        switch (instruction) {
#pragma region 8_BIT_TRANSFER
            //LD A A
            case 0b01111111:
                ++PC;
                break;
            //LD A B
            case 0b01111000:
                A = B;
                ++PC;
                break;
            //LD A C
            case 0b01111001:
                A = C;
                ++PC;
                break;
            //LD A D
            case 0b01111010:
                A = D;
                ++PC;
                break;
            //LD A E
            case 0b01111011:
                A = E;
                ++PC;
                break;
            //LD A H
            case 0b01111100:
                A = H;
                ++PC;
                break;
            //LD A L
            case 0b01111101:
                A = L;
                ++PC;
                break;
            //LD B A
            case 0b01000111:
                B = A;
                ++PC;
                break;
            //LD B B
            case 0b01000000:
                ++PC;
                break;
            //LD B C
            case 0b01000001:
                B = C;
                ++PC;
                break;
            //LD B D
            case 0b01000010:
                B = D;
                ++PC;
                break;
            //LD B E
            case 0b01000011:
                B = E;
                ++PC;
                break;
            //LD B H
            case 0b01000100:
                B = H;
                ++PC;
                break;
            //LD B L
            case 0b01000101:
                B = L;
                ++PC;
                break;
            //LD C A
            case 0b01001111:
                C = A;
                ++PC;
                break;
            //LD C B
            case 0b01001000:
                C = B;
                ++PC;
                break;
            //LD C C
            case 0b01001001:
                ++PC;
                break;
            //LD C D
            case 0b01001010:
                C = D;
                ++PC;
                break;
            //LD C E
            case 0b01001011:
                C = E;
                ++PC;
                break;
            //LD C H
            case 0b01001100:
                C = H;
                ++PC;
                break;
            //LD C L
            case 0b01001101:
                C = L;
                ++PC;
                break;
            //LD D A
            case 0b01010111:
                D = A;
                ++PC;
                break;
            //LD D B
            case 0b01010000:
                D = B;
                ++PC;
                break;
            //LD D C
            case 0b01010001:
                D = C;
                ++PC;
                break;
            //LD D D
            case 0b01010010:
                ++PC;
                break;
            //LD D E
            case 0b01010011:
                D = E;
                ++PC;
                break;
            //LD D H
            case 0b01010100:
                D = H;
                ++PC;
                break;
            //LD D L
            case 0b01010101:
                D = L;
                ++PC;
                break;
            //LD E A
            case 0b01011111:
                E = A;
                ++PC;
                break;
            //LD E B
            case 0b01011000:
                E = B;
                ++PC;
                break;
            //LD E C
            case 0b01011001:
                E = C;
                ++PC;
                break;
            //LD E D
            case 0b01011010:
                E = D;
                ++PC;
                break;
            //LD E E
            case 0b01011011:
                ++PC;
                break;
            //LD E H
            case 0b01011100:
                E = H;
                ++PC;
                break;
            //LD E L
            case 0b01011101:
                E = L;
                ++PC;
                break;
            //LD H A
            case 0b01100111:
                H = A;
                ++PC;
                break;
            //LD H B
            case 0b01100000:
                H = B;
                ++PC;
                break;
            //LD H C
            case 0b01100001:
                H = C;
                ++PC;
                break;
            //LD H D
            case 0b01100010:
                H = D;
                ++PC;
                break;
            //LD H E
            case 0b01100011:
                H = E;
                ++PC;
                break;
            //LD H H
            case 0b01100100:
                ++PC;
                break;
            //LD H L
            case 0b01100101:
                H = L;
                ++PC;
                break;
            //LD L A
            case 0b01101111:
                L = A;
                ++PC;
                break;
            //LD L B
            case 0b01101000:
                L = B;
                ++PC;
                break;
            //LD L C
            case 0b01101001:
                L = C;
                ++PC;
                break;
            //LD L D
            case 0b01101010:
                L = D;
                ++PC;
                break;
            //LD L E
            case 0b01101011:
                L = E;
                ++PC;
                break;
            //LD L H
            case 0b01101100:
                L = H;
                ++PC;
                break;
            //LD L L
            case 0b01101101:
                ++PC;
                break;
            //LD A n
            case 0b00111110:
                ++PC;
                A = bus->read(PC);
                ++cycleToExecute;
                ++PC;
                break;
            //LD B n
            case 0b00000110:
                ++PC;
                B = bus->read(PC);
                ++cycleToExecute;
                ++PC;
                break;
            //LD C n
            case 0b00001110:
                ++PC;
                C = bus->read(PC);
                ++cycleToExecute;
                ++PC;
                break;
            //LD D n
            case 0b00010110:
                ++PC;
                D = bus->read(PC);
                ++cycleToExecute;
                ++PC;
                break;
            //LD E n
            case 0b00011110:
                ++PC;
                E = bus->read(PC);
                ++cycleToExecute;
                ++PC;
                break;
            //LD H n
            case 0b00100110:
                ++PC;
                H = bus->read(PC);
                ++cycleToExecute;
                ++PC;
                break;
            //LD L n
            case 0b00101110:
                ++PC;
                L = bus->read(PC);
                ++cycleToExecute;
                ++PC;
                break;
            //LD A (HL)
            case 0b01111110:
                A = bus->read(H << 8 | L);
                ++cycleToExecute;
                ++PC;
                break;
            //LD B (HL)
            case 0b01000110:
                B = bus->read(H << 8 | L);
                ++cycleToExecute;
                ++PC;
                break;
            //LD C (HL)
            case 0b01001110:
                C = bus->read(H << 8 | L);
                ++cycleToExecute;
                ++PC;
                break;
            //LD D (HL)
            case 0b01010110:
                D = bus->read(H << 8 | L);
                ++cycleToExecute;
                ++PC;
                break;
            //LD E (HL)
            case 0b01011110:
                E = bus->read(H << 8 | L);
                ++cycleToExecute;
                ++PC;
                break;
            //LD H (HL)
            case 0b01100110:
                H = bus->read(H << 8 | L);
                ++cycleToExecute;
                ++PC;
                break;
            //LD L (HL)
            case 0b01101110:
                L = bus->read(H << 8 | L);
                ++cycleToExecute;
                ++PC;
                break;
            //LD (HL) A
            case 0b01110111:
                bus->write(H << 8 | L, A);
                ++cycleToExecute;
                ++PC;
                break;
            //LD (HL) B
            case 0b01110000:
                bus->write(H << 8 | L, B);
                ++cycleToExecute;
                ++PC;
                break;
            //LD (HL) C
            case 0b01110001:
                bus->write(H << 8 | L, C);
                ++cycleToExecute;
                ++PC;
                break;
            //LD (HL) D
            case 0b01110010:
                bus->write(H << 8 | L, D);
                ++cycleToExecute;
                ++PC;
                break;
            //LD (HL) E
            case 0b01110011:
                bus->write(H << 8 | L, E);
                ++cycleToExecute;
                ++PC;
                break;
            //LD (HL) H
            case 0b01110100:
                bus->write(H << 8 | L, H);
                ++cycleToExecute;
                ++PC;
                break;
            //LD (HL) L
            case 0b01110101:
                bus->write(H << 8 | L, L);
                ++cycleToExecute;
                ++PC;
                break;
            //LD (HL) n
            case 0b00110110:
                ++PC;
                bus->write(H << 8 | L, bus->read(PC));
                cycleToExecute += 2;
                ++PC;
            //LD A (BC)
            case 0b00001010:
                A = bus->read(B << 8 | C);
                ++cycleToExecute;
                ++PC;
            //LD A (DE)
            case 0b00011010:
                A = bus->read(D << 8 | E);
                ++cycleToExecute;
                ++PC;
            //LD A (C)
            case 0b11110010:
                A = bus->read(0xFF00 | C);
                ++cycleToExecute;
                ++PC;
            //LD (C) A
            case 0b11100010:
                bus->write(0xFF00 | C, A);
                ++cycleToExecute;
                ++PC;
            //LD A (n)
            case 0b11110000:
                ++PC;
                A = bus->read(0xFF00 | bus->read(PC));
                cycleToExecute += 2;
                ++PC;
            //LD (n) A
            case 0b11100000:
                ++PC;
                bus->write(0xFF00 | bus->read(PC), A);
                cycleToExecute += 2;
                ++PC;
            //LD A (nn)
            case 0b11111010:
                ++PC;
                A = bus->read(bus->read(PC << 8 | bus->read(PC + 1)));
                PC += 2;
                cycleToExecute += 4;
            //LD (nn) A
            case 0b11101010:
                ++PC;
                bus->write(bus->read(PC << 8 | bus->read(PC + 1)), A);
                PC += 2;
                cycleToExecute += 4;
            //LD A (HLI)
            case 0b00101010:
                helpVariable16 = H << 8 | L;
                A = bus->read(helpVariable16);
                ++helpVariable16;
                H = helpVariable16 >> 8;
                L = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
            //LD A (HLD)
            case 0b00111010:
                helpVariable16 = H << 8 | L;
                A = bus->read(helpVariable16);
                --helpVariable16;
                H = helpVariable16 >> 8;
                L = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
            //LD (BC) A
            case 0b00000010:
                bus->write(B << 8 | C, A);
                ++cycleToExecute;
                ++PC;
            //LD (DE) A
            case 0b00010010:
                bus->write(D << 8 | E, A);
                ++cycleToExecute;
                ++PC;
            //LD (HLI) A
            case 0b00100010:
                helpVariable16 = H << 8 | L;
                bus->write(helpVariable16, A);
                ++helpVariable16;
                H = helpVariable16 >> 8;
                L = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
            case 0b00110010:
                //LD (HLD) A
                helpVariable16 = H << 8 | L;
                bus->write(helpVariable16, A);
                --helpVariable16;
                H = helpVariable16 >> 8;
                L = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
#pragma endregion 8_BIT_TRANSFER
#pragma region 16_BIT_TRANSFER
            // LM BC nn
            case 0b00000001:
                ++PC;
                B = bus->read(bus->read(PC));
                ++PC;
                C = bus->read(bus->read(PC));
                ++PC;
                cycleNumber += 2;
                break;
            // LM DE nn
            case 0b00010001:
                ++PC;
                E = bus->read(bus->read(PC));
                ++PC;
                D = bus->read(bus->read(PC));
                ++PC;
                cycleNumber += 2;
                break;
            // LM HL nn
            case 0b00100001:
                ++PC;
                H = bus->read(bus->read(PC));
                ++PC;
                L = bus->read(bus->read(PC));
                ++PC;
                cycleNumber += 2;
                break;
            // LM SP nn
            case 0b00110001:
                ++PC;
                SP = bus->read(bus->read(PC)) | bus->read(bus->read(PC + 1)) << 8;
                PC += 2;
                cycleNumber += 2;
                break;
            //LD SP HL
            case 0b011111001:
                SP = H << 8 | L;
                PC += 2;
                ++cycleNumber;
                break;
            //PUSH BC
            case 0b11000101:
                --SP;
                bus->write(SP, B);
                --SP;
                bus->write(SP, C);
                ++PC;
                cycleToExecute += 3;
                break;
            //PUSH DE
            case 0b11010101:
                --SP;
                bus->write(SP, D);
                --SP;
                bus->write(SP, E);
                ++PC;
                cycleToExecute += 3;
                break;
            //PUSH HL
            case 0b11100101:
                --SP;
                bus->write(SP, H);
                --SP;
                bus->write(SP, L);
                ++PC;
                cycleToExecute += 3;
                break;
            //PUSH AF
            case 0b11110101:
                --SP;
                bus->write(SP, A);
                --SP;
                bus->write(SP, F);
                ++PC;
                cycleToExecute += 3;
                break;
            //POP BC
            case 0b11000001:
                C = bus->read(bus->read(SP));
                ++SP;
                B = bus->read(bus->read(SP));
                ++SP;
                ++PC;
                cycleToExecute += 2;
                break;
            //POP DE
            case 0b11010001:
                E = bus->read(bus->read(SP));
                ++SP;
                D = bus->read(bus->read(SP));
                ++SP;
                ++PC;
                cycleToExecute += 2;
                break;
            //POP HL
            case 0b11100001:
                L = bus->read(bus->read(SP));
                ++SP;
                H = bus->read(bus->read(SP));
                ++SP;
                ++PC;
                cycleToExecute += 2;
                break;
            //POP AF
            case 0b11110001:
                F = bus->read(bus->read(SP));
                ++SP;
                A = bus->read(bus->read(SP));
                ++SP;
                ++PC;
                cycleToExecute += 2;
                break;
            //LDHL SP e
            case 0b11111000:
                helpVariable8 = bus->read(bus->read(PC));
                helpVariable16 = SP + static_cast<int8_t>(helpVariable8);
                F = (SP > helpVariable16) << 4 | ((SP & 0x0F) + (helpVariable8 & 0x0F) > 0x0F) << 5;
                H = helpVariable16 >> 8;
                L = helpVariable16 & 0xFF;
                cycleToExecute += 2;
            //LD (nn) SP
            case 0b00001000:
                ++PC;
                bus->write(bus->read(PC), SP & 0xFF);
                ++PC;
                bus->write(bus->read(PC), SP >> 8);
                ++PC;
                cycleToExecute += 4;
#pragma endregion 16_BIT_TRANSFER
#pragma region 8_BIT_ARITHMETIC
            //ADD A A
            case 0b10000111:
                A = arithmetic_8<ADD>(A, A, F);
                ++PC;
                break;
            //ADD A B
            case 0b10000000:
                A = arithmetic_8<ADD>(A, B, F);
                ++PC;
                break;
            //ADD A C
            case 0b10000001:
                A = arithmetic_8<ADD>(A, C, F);
                ++PC;
                break;
            //ADD A D
            case 0b10000010:
                A = arithmetic_8<ADD>(A, D, F);
                ++PC;
                break;
            //ADD A E
            case 0b10000011:
                A = arithmetic_8<ADD>(A, E, F);
                ++PC;
                break;
            //ADD A H
            case 0b10000100:
                A = arithmetic_8<ADD>(A, H, F);
                ++PC;
                break;
            //ADD A L
            case 0b10000101:
                A = arithmetic_8<ADD>(A, L, F);
                ++PC;
                break;
            //ADD A n
            case 0b11000110:
                ++PC;
                A = arithmetic_8<ADD>(A, bus->read(PC), F);
                ++PC;
                ++cycleToExecute;
                break;
            //ADD A (HL)
            case 0b10000110:
                helpVariable16 = H << 8 | L;
                A = arithmetic_8<ADD>(A, bus->read(helpVariable16), F);
                ++PC;
                ++cycleToExecute;
                break;
            //ADC A A
            case 0b10001111:
                A = arithmetic_8<ADC>(A, A, F);
                ++PC;
                break;
            //ADC A B
            case 0b10001000:
                A = arithmetic_8<ADC>(A, B, F);
                ++PC;
                break;
            //ADC A C
            case 0b10001001:
                A = arithmetic_8<ADC>(A, C, F);
                ++PC;
                break;
            //ADC A D
            case 0b10001010:
                A = arithmetic_8<ADC>(A, D, F);
                ++PC;
                break;
            //ADC A E
            case 0b10001011:
                A = arithmetic_8<ADC>(A, E, F);
                ++PC;
                break;
            //ADC A H
            case 0b10001100:
                A = arithmetic_8<ADC>(A, H, F);
                ++PC;
                break;
            //ADC A L
            case 0b10001101:
                A = arithmetic_8<ADC>(A, L, F);
                ++PC;
                break;
            //ADC A n
            case 0b11001110:
                ++PC;
                A = arithmetic_8<ADC>(A, bus->read(PC), F);
                ++PC;
                ++cycleToExecute;
                break;
            //ADC A (HL)
            case 0b10001110:
                helpVariable16 = H << 8 | L;
                A = arithmetic_8<ADC>(A, bus->read(helpVariable16), F);
                ++PC;
                ++cycleToExecute;
                break;
            //SUB A A
            case 0b10010111:
                A = arithmetic_8<SUB>(A, A, F);
                ++PC;
                break;
            //SUB A B
            case 0b10010000:
                A = arithmetic_8<SUB>(A, B, F);
                ++PC;
                break;
            //SUB A C
            case 0b10010001:
                A = arithmetic_8<SUB>(A, C, F);
                ++PC;
                break;
            //SUB A D
            case 0b10010010:
                A = arithmetic_8<SUB>(A, D, F);
                ++PC;
                break;
            //SUB A E
            case 0b10010011:
                A = arithmetic_8<SUB>(A, E, F);
                ++PC;
                break;
            //SUB A H
            case 0b10010100:
                A = arithmetic_8<SUB>(A, H, F);
                ++PC;
                break;
            //SUB A L
            case 0b10010101:
                A = arithmetic_8<SUB>(A, L, F);
                ++PC;
                break;
            //SUB A n
            case 0b11010110:
                ++PC;
                A = arithmetic_8<SUB>(A, bus->read(PC), F);
                ++PC;
                ++cycleToExecute;
                break;
            //SUB A (HL)
            case 0b10010110:
                helpVariable16 = H << 8 | L;
                A = arithmetic_8<SUB>(A, bus->read(helpVariable16), F);
                ++PC;
                ++cycleToExecute;
                break;
            //SBC A A
            case 0b10011111:
                A = arithmetic_8<SBC>(A, A, F);
                ++PC;
                break;
            //SBC A B
            case 0b10011000:
                A = arithmetic_8<SBC>(A, B, F);
                ++PC;
                break;
            //SBC A C
            case 0b10011001:
                A = arithmetic_8<SBC>(A, C, F);
                ++PC;
                break;
            //SBC A D
            case 0b10011010:
                A = arithmetic_8<SBC>(A, D, F);
                ++PC;
                break;
            //SBC A E
            case 0b10011011:
                A = arithmetic_8<SBC>(A, E, F);
                ++PC;
                break;
            //SBC A H
            case 0b10011100:
                A = arithmetic_8<SBC>(A, H, F);
                ++PC;
                break;
            //SBC A L
            case 0b10011101:
                A = arithmetic_8<SBC>(A, L, F);
                ++PC;
                break;
            //SBC A n
            case 0b11011110:
                ++PC;
                A = arithmetic_8<SBC>(A, bus->read(PC), F);
                ++PC;
                ++cycleToExecute;
                break;
            //SBC A (HL)
            case 0b10011110:
                helpVariable16 = H << 8 | L;
                A = arithmetic_8<SUB>(A, bus->read(helpVariable16), F);
                ++PC;
                ++cycleToExecute;
                break;
            //AND A A
            case 0b10100111:
                A = arithmetic_8<AND>(A, A, F);
                ++PC;
                break;
            //AND A B
            case 0b10100000:
                A = arithmetic_8<AND>(A, B, F);
                ++PC;
                break;
            //AND A C
            case 0b10100001:
                A = arithmetic_8<AND>(A, C, F);
                ++PC;
                break;
            //AND A D
            case 0b10100010:
                A = arithmetic_8<AND>(A, D, F);
                ++PC;
                break;
            //AND A E
            case 0b10100011:
                A = arithmetic_8<AND>(A, E, F);
                ++PC;
                break;
            //AND A H
            case 0b10100100:
                A = arithmetic_8<AND>(A, H, F);
                ++PC;
                break;
            //AND A L
            case 0b10100101:
                A = arithmetic_8<AND>(A, L, F);
                ++PC;
                break;
            //AND A n
            case 0b11100110:
                ++PC;
                A = arithmetic_8<AND>(A, bus->read(PC), F);
                A &= bus->read(PC);
                ++PC;
                ++cycleToExecute;
                break;
            //AND A (HL)
            case 0b10100110:
                helpVariable16 = H << 8 | L;
                A = arithmetic_8<AND>(A, bus->read(helpVariable16), F);
                ++PC;
                ++cycleToExecute;
                break;
            //OR A A
            case 0b10110111:
                A = arithmetic_8<OR>(A, A, F);
                ++PC;
                break;
            //OR A B
            case 0b10110000:
                A = arithmetic_8<OR>(A, B, F);
                ++PC;
                break;
            //OR A C
            case 0b10110001:
                A = arithmetic_8<OR>(A, C, F);
                ++PC;
                break;
            //OR A D
            case 0b10110010:
                A = arithmetic_8<OR>(A, D, F);
                ++PC;
                break;
            //OR A E
            case 0b10110011:
                A = arithmetic_8<OR>(A, E, F);
                ++PC;
                break;
            //OR A H
            case 0b10110100:
                A = arithmetic_8<OR>(A, H, F);
                ++PC;
                break;
            //OR A L
            case 0b10110101:
                A = arithmetic_8<OR>(A, L, F);
                ++PC;
                break;
            //OR A n
            case 0b11110110:
                ++PC;
                A = arithmetic_8<OR>(A, bus->read(PC), F);
                A &= bus->read(PC);
                ++PC;
                ++cycleToExecute;
                break;
            //OR A (HL)
            case 0b10110110:
                helpVariable16 = H << 8 | L;
                A = arithmetic_8<OR>(A, bus->read(helpVariable16), F);
                ++PC;
                ++cycleToExecute;
                break;
            //XOR A A
            case 0b10101111:
                A = arithmetic_8<XOR>(A, A, F);
                ++PC;
                break;
            //XOR A B
            case 0b10101000:
                A = arithmetic_8<XOR>(A, B, F);
                ++PC;
                break;
            //XOR A C
            case 0b10101001:
                A = arithmetic_8<XOR>(A, C, F);
                ++PC;
                break;
            //XOR A D
            case 0b10101010:
                A = arithmetic_8<XOR>(A, D, F);
                ++PC;
                break;
            //XOR A E
            case 0b10101011:
                A = arithmetic_8<XOR>(A, E, F);
                ++PC;
                break;
            //XOR A H
            case 0b10101100:
                A = arithmetic_8<XOR>(A, H, F);
                ++PC;
                break;
            //XOR A L
            case 0b10101101:
                A = arithmetic_8<XOR>(A, L, F);
                ++PC;
                break;
            //XOR A n
            case 0b11101110:
                ++PC;
                A = arithmetic_8<XOR>(A, bus->read(PC), F);
                A &= bus->read(PC);
                ++PC;
                ++cycleToExecute;
                break;
            //XOR A (HL)
            case 0b10101110:
                helpVariable16 = H << 8 | L;
                A = arithmetic_8<XOR>(A, bus->read(helpVariable16), F);
                ++PC;
                ++cycleToExecute;
                break;
            //CP A A
            case 0b10111111:
                A = arithmetic_8<CP>(A, A, F);
                ++PC;
                break;
            //CP A B
            case 0b10111000:
                A = arithmetic_8<CP>(A, B, F);
                ++PC;
                break;
            //CP A C
            case 0b10111001:
                A = arithmetic_8<CP>(A, C, F);
                ++PC;
                break;
            //CP A D
            case 0b10111010:
                A = arithmetic_8<CP>(A, D, F);
                ++PC;
                break;
            //CP A E
            case 0b10111011:
                A = arithmetic_8<CP>(A, E, F);
                ++PC;
                break;
            //CP A H
            case 0b10111100:
                A = arithmetic_8<CP>(A, H, F);
                ++PC;
                break;
            //CP A L
            case 0b10111101:
                A = arithmetic_8<CP>(A, L, F);
                ++PC;
                break;
            //CP A n
            case 0b11111110:
                ++PC;
                A = arithmetic_8<CP>(A, bus->read(PC), F);
                A &= bus->read(PC);
                ++PC;
                ++cycleToExecute;
                break;
            //CP A (HL)
            case 0b10111110:
                helpVariable16 = H << 8 | L;
                A = arithmetic_8<CP>(A, bus->read(helpVariable16), F);
                ++PC;
                ++cycleToExecute;
                break;
            //INC A
            case 0b00111100:
                A = arithmetic_8<INC>(A, 1, F);
                ++PC;
                break;
            //INC B
            case 0b00000100:
                B = arithmetic_8<INC>(B, 1, F);
                ++PC;
                break;
            //INC C
            case 0b00001100:
                C = arithmetic_8<INC>(C, 1, F);
                ++PC;
                break;
            //INC D
            case 0b00010100:
                D = arithmetic_8<INC>(D, 1, F);
                ++PC;
                break;
            //INC E
            case 0b00011100:
                E = arithmetic_8<INC>(E, 1, F);
                ++PC;
                break;
            //INC H
            case 0b00100100:
                H = arithmetic_8<INC>(H, 1, F);
                ++PC;
                break;
            //INC L
            case 0b00101100:
                L = arithmetic_8<INC>(L, 1, F);
                ++PC;
                break;
            //INC (HL)
            case 0b00110100:
                helpVariable16 = H << 8 | L;
                bus->write(helpVariable16, arithmetic_8<INC>(bus->read(helpVariable16), 1, F));
                cycleToExecute += 2;
                ++PC;
                break;
            //DEC A
            case 0b00111101:
                A = arithmetic_8<DEC>(A, 1, F);
                ++PC;
                break;
            //DEC B
            case 0b00000101:
                B = arithmetic_8<DEC>(B, 1, F);
                ++PC;
                break;
            //DEC C
            case 0b00001101:
                C = arithmetic_8<DEC>(C, 1, F);
                ++PC;
                break;
            //DEC D
            case 0b00010101:
                D = arithmetic_8<DEC>(D, 1, F);
                ++PC;
                break;
            //DEC E
            case 0b00011101:
                E = arithmetic_8<DEC>(E, 1, F);
                ++PC;
                break;
            //DEC H
            case 0b00100101:
                H = arithmetic_8<DEC>(H, 1, F);
                ++PC;
                break;
            //DEC L
            case 0b00101101:
                L = arithmetic_8<DEC>(L, 1, F);
                ++PC;
                break;
            //DEC (HL)
            case 0b00110101:
                helpVariable16 = H << 8 | L;
                bus->write(helpVariable16, arithmetic_8<DEC>(bus->read(helpVariable16), 1, F));
                cycleToExecute += 2;
                ++PC;
                break;


#pragma endregion 8_BIT_ARITHMETIC
#pragma region 16_BIT_ARITHMETIC
            // ADD HL BC
            case 0b00001001:
                helpVariable16 = arithmetic_16<ADD>(H << 8 | L, B << 8 | C, F);
                H = helpVariable16 >> 8;
                L = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            // ADD HL DE
            case 0b00011001:
                helpVariable16 = arithmetic_16<ADD>(H << 8 | L, D << 8 | E, F);
                H = helpVariable16 >> 8;
                L = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            // ADD HL HL
            case 0b00101001:
                helpVariable16 = H << 8 | L;
                helpVariable16 = arithmetic_16<ADD>(helpVariable16, helpVariable16, F);
                H = helpVariable16 >> 8;
                L = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            // ADD HL SP
            case 0b00111001:
                helpVariable16 = arithmetic_16<ADD>(H << 8 | L, SP, F);
                H = helpVariable16 >> 8;
                L = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            //ADD SP e
            case 0b11101000:
                ++PC;
                F &= 0b00111111;
                SP = arithmetic_16<ADD>(SP, bus->read(PC), F);
                ++PC;
                cycleToExecute += 3;
                break;
            //INC BC
            case 0b00000011:
                helpVariable16 = arithmetic_16<INC>(B << 8 | C, 1, F);
                B = helpVariable16 >> 8;
                C = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            //INC DE
            case 0b00010011:
                helpVariable16 = arithmetic_16<INC>(D << 8 | E, 1, F);
                D = helpVariable16 >> 8;
                E = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            //INC HL
            case 0b00100011:
                helpVariable16 = arithmetic_16<INC>(H << 8 | L, 1, F);
                H = helpVariable16 >> 8;
                L = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            //INC SP
            case 0b00110011:
                SP = arithmetic_16<INC>(SP, 1, F);
                ++PC;
                ++cycleToExecute;
                break;
            //DEC BC
            case 0b00001011:
                helpVariable16 = arithmetic_16<DEC>(B << 8 | C, 1, F);
                B = helpVariable16 >> 8;
                C = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            //DEC DE
            case 0b00011011:
                helpVariable16 = arithmetic_16<DEC>(D << 8 | E, 1, F);
                D = helpVariable16 >> 8;
                E = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            //DEC HL
            case 0b00101011:
                helpVariable16 = arithmetic_16<DEC>(H << 8 | L, 1, F);
                H = helpVariable16 >> 8;
                L = helpVariable16 & 0xFF;
                ++PC;
                ++cycleToExecute;
                break;
            //DEC SP
            case 0b00111011:
                SP = arithmetic_16<DEC>(SP, 1, F);
                ++PC;
                ++cycleToExecute;
                break;


#pragma endregion 16_BIT_ARITHMETIC
#pragma region ROTATION_SHIFT
            //RLCA
            case 0b00000111:
                A = shift_rotation<RLC>(A, F);
                ++PC;
                break;
            //RLA
            case 0b00010111:
                A = shift_rotation<RL>(A, F);
                ++PC;
                break;
            //RRCA
            case 0b00001111:
                A = shift_rotation<RRC>(A, F);
                ++PC;
                break;
            //RRA
            case 0b00011111:
                A = shift_rotation<RR>(A, F);
                ++PC;
                break;
            // double line instruction
            case 0b11001011:
                ++PC;
                switch (bus->read(PC)) {
                    //RLC A
                    case 0b00000111:
                        A = shift_rotation<RLC>(A, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RLC B
                    case 0b00000000:
                        B = shift_rotation<RLC>(B, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RLC C
                    case 0b00000001:
                        C = shift_rotation<RLC>(C, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RLC D
                    case 0b00000010:
                        D = shift_rotation<RLC>(D, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RLC E
                    case 0b00000011:
                        E = shift_rotation<RLC>(E, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RLC H
                    case 0b00000100:
                        H = shift_rotation<RLC>(H, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RLC L
                    case 0b00000101:
                        L = shift_rotation<RLC>(L, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RLC (HL)
                    case 0b00000110:
                        helpVariable16 = H << 8 | L;
                        bus->write(shift_rotation<RLC>(bus->read(helpVariable16), F), helpVariable16);
                        ++PC;
                        cycleToExecute += 3;
                        break;
                    //RL A
                    case 0b00010111:
                        A = shift_rotation<RL>(A, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RL B
                    case 0b00010000:
                        B = shift_rotation<RL>(B, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RL C
                    case 0b00010001:
                        C = shift_rotation<RL>(C, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RL D
                    case 0b00010010:
                        D = shift_rotation<RL>(D, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RL E
                    case 0b00010011:
                        E = shift_rotation<RL>(E, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RL H
                    case 0b00010100:
                        H = shift_rotation<RL>(H, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RL L
                    case 0b00010101:
                        L = shift_rotation<RL>(L, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RL (HL)
                    case 0b00010110:
                        helpVariable16 = H << 8 | L;
                        bus->write(shift_rotation<RL>(bus->read(helpVariable16), F), helpVariable16);
                        ++PC;
                        cycleToExecute += 3;
                        break;
                    //RRC A
                    case 0b00001111:
                        A = shift_rotation<RRC>(A, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RRC B
                    case 0b00001000:
                        B = shift_rotation<RRC>(B, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RRC C
                    case 0b00001001:
                        C = shift_rotation<RRC>(C, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RRC D
                    case 0b00001010:
                        D = shift_rotation<RRC>(D, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RRC E
                    case 0b00001011:
                        E = shift_rotation<RRC>(E, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RRC H
                    case 0b00001100:
                        H = shift_rotation<RRC>(H, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RRC L
                    case 0b00001101:
                        L = shift_rotation<RRC>(L, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RRC (HL)
                    case 0b00001110:
                        helpVariable16 = H << 8 | L;
                        bus->write(shift_rotation<RRC>(bus->read(helpVariable16), F), helpVariable16);
                        ++PC;
                        cycleToExecute += 3;
                        break;
                    //RR A
                    case 0b00011111:
                        A = shift_rotation<RR>(A, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RR B
                    case 0b00011000:
                        B = shift_rotation<RR>(B, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RR C
                    case 0b00011001:
                        C = shift_rotation<RR>(C, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RR D
                    case 0b00011010:
                        D = shift_rotation<RR>(D, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RR E
                    case 0b00011011:
                        E = shift_rotation<RR>(E, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RR H
                    case 0b00011100:
                        H = shift_rotation<RR>(H, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RR L
                    case 0b00011101:
                        L = shift_rotation<RR>(L, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //RR (HL)
                    case 0b00011110:
                        helpVariable16 = H << 8 | L;
                        bus->write(shift_rotation<RR>(bus->read(helpVariable16), F), helpVariable16);
                        ++PC;
                        cycleToExecute += 3;
                        break;
                    //SLA A
                    case 0b00100111:
                        A = shift_rotation<SLA>(A, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SLA B
                    case 0b00100000:
                        B = shift_rotation<SLA>(B, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SLA C
                    case 0b00100001:
                        C = shift_rotation<SLA>(C, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SLA D
                    case 0b00100010:
                        D = shift_rotation<SLA>(D, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SLA E
                    case 0b00100011:
                        E = shift_rotation<SLA>(E, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SLA H
                    case 0b00100100:
                        H = shift_rotation<SLA>(H, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SLA L
                    case 0b00100101:
                        L = shift_rotation<SLA>(L, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SLA (HL)
                    case 0b00100110:
                        helpVariable16 = H << 8 | L;
                        bus->write(shift_rotation<SLA>(bus->read(helpVariable16), F), helpVariable16);
                        ++PC;
                        cycleToExecute += 3;
                        break;
                    //SRA A
                    case 0b00101111:
                        A = shift_rotation<SRA>(A, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRA B
                    case 0b00101000:
                        B = shift_rotation<SRA>(B, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRA C
                    case 0b00101001:
                        C = shift_rotation<SRA>(C, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRA D
                    case 0b00101010:
                        D = shift_rotation<SRA>(D, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRA E
                    case 0b00101011:
                        E = shift_rotation<SRA>(E, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRA H
                    case 0b00101100:
                        H = shift_rotation<SRA>(H, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRA L
                    case 0b00101101:
                        L = shift_rotation<SRA>(L, F);
                        ++PC;
                        ++cycleToExecute;
                        break;
                    //SRA (HL)
                    case 0b00101110:
                        helpVariable16 = H << 8 | L;
                        bus->write(shift_rotation<SRA>(bus->read(helpVariable16), F), helpVariable16);
                        ++PC;
                        cycleToExecute += 3;
                        break;
                    //SRL A
                    case 0b00111111:
                        A = shift_rotation<SRL>(A, F);
                    ++PC;
                    ++cycleToExecute;
                    break;
                    //SRL B
                    case 0b00111000:
                        B = shift_rotation<SRL>(B, F);
                    ++PC;
                    ++cycleToExecute;
                    break;
                    //SRL C
                    case 0b00111001:
                        C = shift_rotation<SRL>(C, F);
                    ++PC;
                    ++cycleToExecute;
                    break;
                    //SRL D
                    case 0b00111010:
                        D = shift_rotation<SRL>(D, F);
                    ++PC;
                    ++cycleToExecute;
                    break;
                    //SRL E
                    case 0b00111011:
                        E = shift_rotation<SRL>(E, F);
                    ++PC;
                    ++cycleToExecute;
                    break;
                    //SRL H
                    case 0b00111100:
                        H = shift_rotation<SRL>(H, F);
                    ++PC;
                    ++cycleToExecute;
                    break;
                    //SRL L
                    case 0b00111101:
                        L = shift_rotation<SRL>(L, F);
                    ++PC;
                    ++cycleToExecute;
                    break;
                    //SRL (HL)
                    case 0b00111110:
                        helpVariable16 = H << 8 | L;
                    bus->write(shift_rotation<SRL>(bus->read(helpVariable16), F), helpVariable16);
                    ++PC;
                    cycleToExecute += 3;
                    break;
                    //SWAP A
                    case 0b00110111:
                        A = shift_rotation<SRL>(A, F);
                    ++PC;
                    ++cycleToExecute;
                    break;
                    //SWAP B
                    case 0b00110000:
                        B = shift_rotation<SRL>(B, F);
                    ++PC;
                    ++cycleToExecute;
                    break;
                    //SWAP C
                    case 0b00110001:
                        C = shift_rotation<SRL>(C, F);
                    ++PC;
                    ++cycleToExecute;
                    break;
                    //SWAP D
                    case 0b00110010:
                        D = shift_rotation<SRL>(D, F);
                    ++PC;
                    ++cycleToExecute;
                    break;
                    //SWAP E
                    case 0b00110011:
                        E = shift_rotation<SRL>(E, F);
                    ++PC;
                    ++cycleToExecute;
                    break;
                    //SWAP H
                    case 0b00110100:
                        H = shift_rotation<SRL>(H, F);
                    ++PC;
                    ++cycleToExecute;
                    break;
                    //SWAP L
                    case 0b00110101:
                        L = shift_rotation<SRL>(L, F);
                    ++PC;
                    ++cycleToExecute;
                    break;
                    //SWAP (HL)
                    case 0b00110110:
                        helpVariable16 = H << 8 | L;
                    bus->write(shift_rotation<SRL>(bus->read(helpVariable16), F), helpVariable16);
                    ++PC;
                    cycleToExecute += 3;
                    break;
#pragma endregion ROTATION_SHIFT
#pragma region BIT_OPERATORS
                    default:
                        switch (instruction>>6) {
                            //BIT
                            case 0b01:
                                break;
                            //SET
                            case 0b11:
                                break;
                            //RES
                            case 0b10:
                                break;
                        }

                    // BIT b A
                }
#pragma endregion BIT_OPERATORS


            default:
                std::cout << "Invalid instruction :" << bus->read(PC) << "\n";;
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
