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
        SWAP
    };

    class Cpu {
        uint8_t A;
        uint8_t F;
        uint8_t B;
        uint8_t C;
        uint8_t D;
        uint8_t E;
        uint8_t H;
        uint8_t L;
        uint16_t SP;
        uint16_t PC;
        BUS::Bus *bus;
        uint64_t cycleNumber;
        uint8_t cycleToExecute;
        bool doubleSpeed{};
        public:
        Cpu(BUS::Bus &bus);

        void executeCycle();

        void executeNCycles(uint32_t n);

        void run();



    };
}



#endif //CPU_H
