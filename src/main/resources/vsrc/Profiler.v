module ProfilerBlackBox #(
  parameter NUM_WARPS = 8,
  parameter COUNTER_WIDTH = 64,
  parameter CLUSTER_ID = 0,
  parameter CORE_ID = 0
) (
  input clock,
  input reset,
  input logic                     finished,
  input logic [COUNTER_WIDTH-1:0] instRetired,
  input logic [COUNTER_WIDTH-1:0] cycles,
  input logic [COUNTER_WIDTH-1:0] cyclesDecoded,
  input logic [COUNTER_WIDTH-1:0] cyclesEligible,
  input logic [COUNTER_WIDTH-1:0] cyclesIssued,
  input logic [(NUM_WARPS*COUNTER_WIDTH)-1:0] perWarp_cyclesDecoded,
  input logic [(NUM_WARPS*COUNTER_WIDTH)-1:0] perWarp_cyclesIssued,
  input logic [(NUM_WARPS*COUNTER_WIDTH)-1:0] perWarp_stallsWAW,
  input logic [(NUM_WARPS*COUNTER_WIDTH)-1:0] perWarp_stallsWAR,
  input logic [(NUM_WARPS*COUNTER_WIDTH)-1:0] perWarp_stallsScoreboard,
  input logic [(NUM_WARPS*COUNTER_WIDTH)-1:0] perWarp_stallsBusy,
  input logic [(NUM_WARPS*COUNTER_WIDTH)-1:0] perWarp_stallsBusyLSU,
  input logic [(NUM_WARPS*COUNTER_WIDTH)-1:0] perWarp_unoccupied,
  input logic [(NUM_WARPS*COUNTER_WIDTH)-1:0] perWarp_idle,
  input logic [(NUM_WARPS*COUNTER_WIDTH)-1:0] perWarp_ifetch,
  input logic [(NUM_WARPS*COUNTER_WIDTH)-1:0] perWarp_control,
  input logic [(NUM_WARPS*COUNTER_WIDTH)-1:0] perWarp_sync,
  input logic [(NUM_WARPS*COUNTER_WIDTH)-1:0] perWarp_memData,
  input logic [(NUM_WARPS*COUNTER_WIDTH)-1:0] perWarp_comData,
  input logic [(NUM_WARPS*COUNTER_WIDTH)-1:0] perWarp_struct
);

  `include "Cyclotron.vh"

  longint per_warp_cycles_decoded [0:NUM_WARPS-1];
  longint per_warp_cycles_issued [0:NUM_WARPS-1];
  longint per_warp_stalls_waw [0:NUM_WARPS-1];
  longint per_warp_stalls_war [0:NUM_WARPS-1];
  longint per_warp_stalls_scoreboard [0:NUM_WARPS-1];
  longint per_warp_stalls_busy [0:NUM_WARPS-1];
  longint per_warp_stalls_busy_lsu [0:NUM_WARPS-1];
  longint per_warp_unoccupied [0:NUM_WARPS-1];
  longint per_warp_idle [0:NUM_WARPS-1];
  longint per_warp_ifetch [0:NUM_WARPS-1];
  longint per_warp_control [0:NUM_WARPS-1];
  longint per_warp_sync [0:NUM_WARPS-1];
  longint per_warp_mem_data [0:NUM_WARPS-1];
  longint per_warp_com_data [0:NUM_WARPS-1];
  longint per_warp_struct [0:NUM_WARPS-1];

  genvar i;
  generate
    for (i = 0; i < NUM_WARPS; i = i + 1) begin
      assign per_warp_cycles_decoded[i] = perWarp_cyclesDecoded[i*COUNTER_WIDTH +: COUNTER_WIDTH];
      assign per_warp_cycles_issued[i] = perWarp_cyclesIssued[i*COUNTER_WIDTH +: COUNTER_WIDTH];
      assign per_warp_stalls_waw[i] = perWarp_stallsWAW[i*COUNTER_WIDTH +: COUNTER_WIDTH];
      assign per_warp_stalls_war[i] = perWarp_stallsWAR[i*COUNTER_WIDTH +: COUNTER_WIDTH];
      assign per_warp_stalls_scoreboard[i] = perWarp_stallsScoreboard[i*COUNTER_WIDTH +: COUNTER_WIDTH];
      assign per_warp_stalls_busy[i] = perWarp_stallsBusy[i*COUNTER_WIDTH +: COUNTER_WIDTH];
      assign per_warp_stalls_busy_lsu[i] = perWarp_stallsBusyLSU[i*COUNTER_WIDTH +: COUNTER_WIDTH];
      assign per_warp_unoccupied[i] = perWarp_unoccupied[i*COUNTER_WIDTH +: COUNTER_WIDTH];
      assign per_warp_idle[i] = perWarp_idle[i*COUNTER_WIDTH +: COUNTER_WIDTH];
      assign per_warp_ifetch[i] = perWarp_ifetch[i*COUNTER_WIDTH +: COUNTER_WIDTH];
      assign per_warp_control[i] = perWarp_control[i*COUNTER_WIDTH +: COUNTER_WIDTH];
      assign per_warp_sync[i] = perWarp_sync[i*COUNTER_WIDTH +: COUNTER_WIDTH];
      assign per_warp_mem_data[i] = perWarp_memData[i*COUNTER_WIDTH +: COUNTER_WIDTH];
      assign per_warp_com_data[i] = perWarp_comData[i*COUNTER_WIDTH +: COUNTER_WIDTH];
      assign per_warp_struct[i] = perWarp_struct[i*COUNTER_WIDTH +: COUNTER_WIDTH];
    end
  endgenerate

  import "DPI-C" function void profile_perf_counters(
    input int     cluster_id,
    input int     core_id,
    input longint inst_retired,
    input longint cycle,
    input longint cycles_decoded,
    input longint cycles_eligible,
    input longint cycles_issued,
    input longint per_warp_cycles_decoded[NUM_WARPS],
    input longint per_warp_cycles_issued[NUM_WARPS],
    input longint per_warp_stalls_waw[NUM_WARPS],
    input longint per_warp_stalls_war[NUM_WARPS],
    input longint per_warp_stalls_scoreboard[NUM_WARPS],
    input longint per_warp_stalls_busy[NUM_WARPS],
    input longint per_warp_stalls_busy_lsu[NUM_WARPS],
    input longint per_warp_unoccupied[NUM_WARPS],
    input longint per_warp_idle[NUM_WARPS],
    input longint per_warp_ifetch[NUM_WARPS],
    input longint per_warp_control[NUM_WARPS],
    input longint per_warp_sync[NUM_WARPS],
    input longint per_warp_mem_data[NUM_WARPS],
    input longint per_warp_com_data[NUM_WARPS],
    input longint per_warp_struct[NUM_WARPS],
    input bit     finished
  );

  initial cyclotron_init_task();

  always @(posedge clock) begin
    if (reset) begin
    end else begin
      profile_perf_counters(
        CLUSTER_ID,
        CORE_ID,
        instRetired,
        cycles,
        cyclesDecoded,
        cyclesEligible,
        cyclesIssued,
        per_warp_cycles_decoded,
        per_warp_cycles_issued,
        per_warp_stalls_waw,
        per_warp_stalls_war,
        per_warp_stalls_scoreboard,
        per_warp_stalls_busy,
        per_warp_stalls_busy_lsu,
        per_warp_unoccupied,
        per_warp_idle,
        per_warp_ifetch,
        per_warp_control,
        per_warp_sync,
        per_warp_mem_data,
        per_warp_com_data,
        per_warp_struct,
        finished
      );
    end
  end

endmodule
