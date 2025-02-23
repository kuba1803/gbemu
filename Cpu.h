//
// Created by Jakub on 16.01.2025.
//

#ifndef CPU_H
#define CPU_H
#include <chrono>
#include <iostream>
#include <thread>

#include "Bus.h"


namespace CPU{

    enum OPERATION_TYPE {
        ADD,
        ADC,
        SUB,
        SBC,
        AND,
        OR,
        XOR,
        CP,
        INC,
        DEC,
        RL,
        RLC,
        RR,
        RRC,
        SLA,
        SRA,
        SRL,
        SWAP,
        BIT,
        SET,
        RES
    };

    enum REGISTER_8_BIT {
        B = 0,
        C = 1,
        D = 2,
        E = 3,
        H = 4,
        L = 5,
        A = 7,
        F = 8,
        PC_HIGH = 9,
        PC_LOW = 10,
        SP_HIGH = 11,
        SP_LOW = 12
    };

    union Registers {
        uint8_t reg_8[13];
        struct {
            uint16_t BC : 16;
            uint16_t DE : 16;
            uint16_t HL : 16;
            uint8_t : 8;
            uint16_t AF  : 16;
            uint16_t PC : 16;
            uint16_t SP : 16;
        };
        struct{
            uint64_t : 64;
            bool cy : 1;
            bool h : 1;
            bool n : 1;
            bool z : 1;
            uint8_t : 36;
        };
    };

    class Cpu {
        Registers registers;
        BUS::Bus *bus;
        uint64_t cycleNumber;
        uint8_t cycleToExecute;
        bool doubleSpeed;
        bool interruptEnabled;
        bool haltMode;
        bool stopMode;
        public:
        Cpu(BUS::Bus &bus);

        void executeCycle();

        void executeNCycles(uint32_t n);

        void run();



    };
}



#endif //CPU_H
