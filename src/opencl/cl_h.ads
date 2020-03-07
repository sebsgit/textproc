pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Strings;

package cl_h is

   CL_SUCCESS : constant := 0;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:137
   CL_DEVICE_NOT_FOUND : constant := -1;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:138
   CL_DEVICE_NOT_AVAILABLE : constant := -2;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:139
   CL_COMPILER_NOT_AVAILABLE : constant := -3;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:140
   CL_MEM_OBJECT_ALLOCATION_FAILURE : constant := -4;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:141
   CL_OUT_OF_RESOURCES : constant := -5;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:142
   CL_OUT_OF_HOST_MEMORY : constant := -6;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:143
   CL_PROFILING_INFO_NOT_AVAILABLE : constant := -7;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:144
   CL_MEM_COPY_OVERLAP : constant := -8;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:145
   CL_IMAGE_FORMAT_MISMATCH : constant := -9;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:146
   CL_IMAGE_FORMAT_NOT_SUPPORTED : constant := -10;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:147
   CL_BUILD_PROGRAM_FAILURE : constant := -11;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:148
   CL_MAP_FAILURE : constant := -12;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:149
   CL_MISALIGNED_SUB_BUFFER_OFFSET : constant := -13;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:150
   CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST : constant := -14;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:151
   CL_COMPILE_PROGRAM_FAILURE : constant := -15;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:152
   CL_LINKER_NOT_AVAILABLE : constant := -16;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:153
   CL_LINK_PROGRAM_FAILURE : constant := -17;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:154
   CL_DEVICE_PARTITION_FAILED : constant := -18;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:155
   CL_KERNEL_ARG_INFO_NOT_AVAILABLE : constant := -19;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:156

   CL_INVALID_VALUE : constant := -30;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:158
   CL_INVALID_DEVICE_TYPE : constant := -31;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:159
   CL_INVALID_PLATFORM : constant := -32;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:160
   CL_INVALID_DEVICE : constant := -33;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:161
   CL_INVALID_CONTEXT : constant := -34;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:162
   CL_INVALID_QUEUE_PROPERTIES : constant := -35;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:163
   CL_INVALID_COMMAND_QUEUE : constant := -36;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:164
   CL_INVALID_HOST_PTR : constant := -37;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:165
   CL_INVALID_MEM_OBJECT : constant := -38;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:166
   CL_INVALID_IMAGE_FORMAT_DESCRIPTOR : constant := -39;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:167
   CL_INVALID_IMAGE_SIZE : constant := -40;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:168
   CL_INVALID_SAMPLER : constant := -41;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:169
   CL_INVALID_BINARY : constant := -42;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:170
   CL_INVALID_BUILD_OPTIONS : constant := -43;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:171
   CL_INVALID_PROGRAM : constant := -44;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:172
   CL_INVALID_PROGRAM_EXECUTABLE : constant := -45;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:173
   CL_INVALID_KERNEL_NAME : constant := -46;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:174
   CL_INVALID_KERNEL_DEFINITION : constant := -47;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:175
   CL_INVALID_KERNEL : constant := -48;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:176
   CL_INVALID_ARG_INDEX : constant := -49;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:177
   CL_INVALID_ARG_VALUE : constant := -50;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:178
   CL_INVALID_ARG_SIZE : constant := -51;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:179
   CL_INVALID_KERNEL_ARGS : constant := -52;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:180
   CL_INVALID_WORK_DIMENSION : constant := -53;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:181
   CL_INVALID_WORK_GROUP_SIZE : constant := -54;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:182
   CL_INVALID_WORK_ITEM_SIZE : constant := -55;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:183
   CL_INVALID_GLOBAL_OFFSET : constant := -56;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:184
   CL_INVALID_EVENT_WAIT_LIST : constant := -57;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:185
   CL_INVALID_EVENT : constant := -58;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:186
   CL_INVALID_OPERATION : constant := -59;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:187
   CL_INVALID_GL_OBJECT : constant := -60;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:188
   CL_INVALID_BUFFER_SIZE : constant := -61;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:189
   CL_INVALID_MIP_LEVEL : constant := -62;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:190
   CL_INVALID_GLOBAL_WORK_SIZE : constant := -63;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:191
   CL_INVALID_PROPERTY : constant := -64;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:192
   CL_INVALID_IMAGE_DESCRIPTOR : constant := -65;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:193
   CL_INVALID_COMPILER_OPTIONS : constant := -66;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:194
   CL_INVALID_LINKER_OPTIONS : constant := -67;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:195
   CL_INVALID_DEVICE_PARTITION_COUNT : constant := -68;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:196
   CL_INVALID_PIPE_SIZE : constant := -69;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:197
   CL_INVALID_DEVICE_QUEUE : constant := -70;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:198

   CL_VERSION_1_0 : constant := 1;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:201
   CL_VERSION_1_1 : constant := 1;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:202
   CL_VERSION_1_2 : constant := 1;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:203
   CL_VERSION_2_0 : constant := 1;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:204
   CL_VERSION_2_1 : constant := 1;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:205

   CL_FALSE : constant := 0;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:208
   CL_TRUE : constant := 1;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:209
   --  unsupported macro: CL_BLOCKING CL_TRUE
   --  unsupported macro: CL_NON_BLOCKING CL_FALSE

   CL_PLATFORM_PROFILE : constant := 16#0900#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:214
   CL_PLATFORM_VERSION : constant := 16#0901#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:215
   CL_PLATFORM_NAME : constant := 16#0902#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:216
   CL_PLATFORM_VENDOR : constant := 16#0903#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:217
   CL_PLATFORM_EXTENSIONS : constant := 16#0904#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:218
   CL_PLATFORM_HOST_TIMER_RESOLUTION : constant := 16#0905#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:219

   CL_DEVICE_TYPE_DEFAULT : constant := (2 ** 0);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:222
   CL_DEVICE_TYPE_CPU : constant := (2 ** 1);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:223
   CL_DEVICE_TYPE_GPU : constant := (2 ** 2);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:224
   CL_DEVICE_TYPE_ACCELERATOR : constant := (2 ** 3);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:225
   CL_DEVICE_TYPE_CUSTOM : constant := (2 ** 4);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:226
   CL_DEVICE_TYPE_ALL : constant := 16#FFFFFFFF#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:227

   CL_DEVICE_TYPE : constant := 16#1000#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:230
   CL_DEVICE_VENDOR_ID : constant := 16#1001#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:231
   CL_DEVICE_MAX_COMPUTE_UNITS : constant := 16#1002#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:232
   CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS : constant := 16#1003#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:233
   CL_DEVICE_MAX_WORK_GROUP_SIZE : constant := 16#1004#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:234
   CL_DEVICE_MAX_WORK_ITEM_SIZES : constant := 16#1005#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:235
   CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR : constant := 16#1006#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:236
   CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT : constant := 16#1007#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:237
   CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT : constant := 16#1008#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:238
   CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG : constant := 16#1009#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:239
   CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT : constant := 16#100A#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:240
   CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE : constant := 16#100B#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:241
   CL_DEVICE_MAX_CLOCK_FREQUENCY : constant := 16#100C#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:242
   CL_DEVICE_ADDRESS_BITS : constant := 16#100D#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:243
   CL_DEVICE_MAX_READ_IMAGE_ARGS : constant := 16#100E#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:244
   CL_DEVICE_MAX_WRITE_IMAGE_ARGS : constant := 16#100F#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:245
   CL_DEVICE_MAX_MEM_ALLOC_SIZE : constant := 16#1010#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:246
   CL_DEVICE_IMAGE2D_MAX_WIDTH : constant := 16#1011#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:247
   CL_DEVICE_IMAGE2D_MAX_HEIGHT : constant := 16#1012#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:248
   CL_DEVICE_IMAGE3D_MAX_WIDTH : constant := 16#1013#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:249
   CL_DEVICE_IMAGE3D_MAX_HEIGHT : constant := 16#1014#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:250
   CL_DEVICE_IMAGE3D_MAX_DEPTH : constant := 16#1015#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:251
   CL_DEVICE_IMAGE_SUPPORT : constant := 16#1016#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:252
   CL_DEVICE_MAX_PARAMETER_SIZE : constant := 16#1017#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:253
   CL_DEVICE_MAX_SAMPLERS : constant := 16#1018#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:254
   CL_DEVICE_MEM_BASE_ADDR_ALIGN : constant := 16#1019#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:255
   CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE : constant := 16#101A#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:256
   CL_DEVICE_SINGLE_FP_CONFIG : constant := 16#101B#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:257
   CL_DEVICE_GLOBAL_MEM_CACHE_TYPE : constant := 16#101C#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:258
   CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE : constant := 16#101D#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:259
   CL_DEVICE_GLOBAL_MEM_CACHE_SIZE : constant := 16#101E#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:260
   CL_DEVICE_GLOBAL_MEM_SIZE : constant := 16#101F#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:261
   CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE : constant := 16#1020#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:262
   CL_DEVICE_MAX_CONSTANT_ARGS : constant := 16#1021#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:263
   CL_DEVICE_LOCAL_MEM_TYPE : constant := 16#1022#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:264
   CL_DEVICE_LOCAL_MEM_SIZE : constant := 16#1023#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:265
   CL_DEVICE_ERROR_CORRECTION_SUPPORT : constant := 16#1024#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:266
   CL_DEVICE_PROFILING_TIMER_RESOLUTION : constant := 16#1025#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:267
   CL_DEVICE_ENDIAN_LITTLE : constant := 16#1026#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:268
   CL_DEVICE_AVAILABLE : constant := 16#1027#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:269
   CL_DEVICE_COMPILER_AVAILABLE : constant := 16#1028#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:270
   CL_DEVICE_EXECUTION_CAPABILITIES : constant := 16#1029#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:271
   CL_DEVICE_QUEUE_PROPERTIES : constant := 16#102A#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:272
   CL_DEVICE_QUEUE_ON_HOST_PROPERTIES : constant := 16#102A#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:273
   CL_DEVICE_NAME : constant := 16#102B#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:274
   CL_DEVICE_VENDOR : constant := 16#102C#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:275
   CL_DRIVER_VERSION : constant := 16#102D#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:276
   CL_DEVICE_PROFILE : constant := 16#102E#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:277
   CL_DEVICE_VERSION : constant := 16#102F#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:278
   CL_DEVICE_EXTENSIONS : constant := 16#1030#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:279
   CL_DEVICE_PLATFORM : constant := 16#1031#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:280
   CL_DEVICE_DOUBLE_FP_CONFIG : constant := 16#1032#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:281

   CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF : constant := 16#1034#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:283
   CL_DEVICE_HOST_UNIFIED_MEMORY : constant := 16#1035#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:284
   CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR : constant := 16#1036#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:285
   CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT : constant := 16#1037#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:286
   CL_DEVICE_NATIVE_VECTOR_WIDTH_INT : constant := 16#1038#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:287
   CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG : constant := 16#1039#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:288
   CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT : constant := 16#103A#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:289
   CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE : constant := 16#103B#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:290
   CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF : constant := 16#103C#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:291
   CL_DEVICE_OPENCL_C_VERSION : constant := 16#103D#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:292
   CL_DEVICE_LINKER_AVAILABLE : constant := 16#103E#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:293
   CL_DEVICE_BUILT_IN_KERNELS : constant := 16#103F#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:294
   CL_DEVICE_IMAGE_MAX_BUFFER_SIZE : constant := 16#1040#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:295
   CL_DEVICE_IMAGE_MAX_ARRAY_SIZE : constant := 16#1041#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:296
   CL_DEVICE_PARENT_DEVICE : constant := 16#1042#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:297
   CL_DEVICE_PARTITION_MAX_SUB_DEVICES : constant := 16#1043#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:298
   CL_DEVICE_PARTITION_PROPERTIES : constant := 16#1044#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:299
   CL_DEVICE_PARTITION_AFFINITY_DOMAIN : constant := 16#1045#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:300
   CL_DEVICE_PARTITION_TYPE : constant := 16#1046#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:301
   CL_DEVICE_REFERENCE_COUNT : constant := 16#1047#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:302
   CL_DEVICE_PREFERRED_INTEROP_USER_SYNC : constant := 16#1048#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:303
   CL_DEVICE_PRINTF_BUFFER_SIZE : constant := 16#1049#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:304
   CL_DEVICE_IMAGE_PITCH_ALIGNMENT : constant := 16#104A#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:305
   CL_DEVICE_IMAGE_BASE_ADDRESS_ALIGNMENT : constant := 16#104B#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:306
   CL_DEVICE_MAX_READ_WRITE_IMAGE_ARGS : constant := 16#104C#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:307
   CL_DEVICE_MAX_GLOBAL_VARIABLE_SIZE : constant := 16#104D#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:308
   CL_DEVICE_QUEUE_ON_DEVICE_PROPERTIES : constant := 16#104E#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:309
   CL_DEVICE_QUEUE_ON_DEVICE_PREFERRED_SIZE : constant := 16#104F#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:310
   CL_DEVICE_QUEUE_ON_DEVICE_MAX_SIZE : constant := 16#1050#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:311
   CL_DEVICE_MAX_ON_DEVICE_QUEUES : constant := 16#1051#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:312
   CL_DEVICE_MAX_ON_DEVICE_EVENTS : constant := 16#1052#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:313
   CL_DEVICE_SVM_CAPABILITIES : constant := 16#1053#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:314
   CL_DEVICE_GLOBAL_VARIABLE_PREFERRED_TOTAL_SIZE : constant := 16#1054#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:315
   CL_DEVICE_MAX_PIPE_ARGS : constant := 16#1055#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:316
   CL_DEVICE_PIPE_MAX_ACTIVE_RESERVATIONS : constant := 16#1056#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:317
   CL_DEVICE_PIPE_MAX_PACKET_SIZE : constant := 16#1057#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:318
   CL_DEVICE_PREFERRED_PLATFORM_ATOMIC_ALIGNMENT : constant := 16#1058#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:319
   CL_DEVICE_PREFERRED_GLOBAL_ATOMIC_ALIGNMENT : constant := 16#1059#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:320
   CL_DEVICE_PREFERRED_LOCAL_ATOMIC_ALIGNMENT : constant := 16#105A#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:321
   CL_DEVICE_IL_VERSION : constant := 16#105B#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:322
   CL_DEVICE_MAX_NUM_SUB_GROUPS : constant := 16#105C#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:323
   CL_DEVICE_SUB_GROUP_INDEPENDENT_FORWARD_PROGRESS : constant := 16#105D#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:324

   CL_FP_DENORM : constant := (2 ** 0);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:327
   CL_FP_INF_NAN : constant := (2 ** 1);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:328
   CL_FP_ROUND_TO_NEAREST : constant := (2 ** 2);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:329
   CL_FP_ROUND_TO_ZERO : constant := (2 ** 3);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:330
   CL_FP_ROUND_TO_INF : constant := (2 ** 4);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:331
   CL_FP_FMA : constant := (2 ** 5);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:332
   CL_FP_SOFT_FLOAT : constant := (2 ** 6);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:333
   CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT : constant := (2 ** 7);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:334

   CL_NONE : constant := 16#0#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:337
   CL_READ_ONLY_CACHE : constant := 16#1#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:338
   CL_READ_WRITE_CACHE : constant := 16#2#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:339

   CL_LOCAL : constant := 16#1#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:342
   CL_GLOBAL : constant := 16#2#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:343

   CL_EXEC_KERNEL : constant := (2 ** 0);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:346
   CL_EXEC_NATIVE_KERNEL : constant := (2 ** 1);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:347

   CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE : constant := (2 ** 0);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:350
   CL_QUEUE_PROFILING_ENABLE : constant := (2 ** 1);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:351
   CL_QUEUE_ON_DEVICE : constant := (2 ** 2);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:352
   CL_QUEUE_ON_DEVICE_DEFAULT : constant := (2 ** 3);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:353

   CL_CONTEXT_REFERENCE_COUNT : constant := 16#1080#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:356
   CL_CONTEXT_DEVICES : constant := 16#1081#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:357
   CL_CONTEXT_PROPERTIES : constant := 16#1082#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:358
   CL_CONTEXT_NUM_DEVICES : constant := 16#1083#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:359

   CL_CONTEXT_PLATFORM : constant := 16#1084#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:362
   CL_CONTEXT_INTEROP_USER_SYNC : constant := 16#1085#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:363

   CL_DEVICE_PARTITION_EQUALLY : constant := 16#1086#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:366
   CL_DEVICE_PARTITION_BY_COUNTS : constant := 16#1087#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:367
   CL_DEVICE_PARTITION_BY_COUNTS_LIST_END : constant := 16#0#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:368
   CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN : constant := 16#1088#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:369

   CL_DEVICE_AFFINITY_DOMAIN_NUMA : constant := (2 ** 0);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:372
   CL_DEVICE_AFFINITY_DOMAIN_L4_CACHE : constant := (2 ** 1);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:373
   CL_DEVICE_AFFINITY_DOMAIN_L3_CACHE : constant := (2 ** 2);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:374
   CL_DEVICE_AFFINITY_DOMAIN_L2_CACHE : constant := (2 ** 3);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:375
   CL_DEVICE_AFFINITY_DOMAIN_L1_CACHE : constant := (2 ** 4);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:376
   CL_DEVICE_AFFINITY_DOMAIN_NEXT_PARTITIONABLE : constant := (2 ** 5);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:377

   CL_DEVICE_SVM_COARSE_GRAIN_BUFFER : constant := (2 ** 0);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:380
   CL_DEVICE_SVM_FINE_GRAIN_BUFFER : constant := (2 ** 1);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:381
   CL_DEVICE_SVM_FINE_GRAIN_SYSTEM : constant := (2 ** 2);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:382
   CL_DEVICE_SVM_ATOMICS : constant := (2 ** 3);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:383

   CL_QUEUE_CONTEXT : constant := 16#1090#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:386
   CL_QUEUE_DEVICE : constant := 16#1091#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:387
   CL_QUEUE_REFERENCE_COUNT : constant := 16#1092#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:388
   CL_QUEUE_PROPERTIES : constant := 16#1093#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:389
   CL_QUEUE_SIZE : constant := 16#1094#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:390
   CL_QUEUE_DEVICE_DEFAULT : constant := 16#1095#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:391

   CL_MEM_READ_WRITE : constant := (2 ** 0);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:394
   CL_MEM_WRITE_ONLY : constant := (2 ** 1);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:395
   CL_MEM_READ_ONLY : constant := (2 ** 2);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:396
   CL_MEM_USE_HOST_PTR : constant := (2 ** 3);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:397
   CL_MEM_ALLOC_HOST_PTR : constant := (2 ** 4);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:398
   CL_MEM_COPY_HOST_PTR : constant := (2 ** 5);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:399

   CL_MEM_HOST_WRITE_ONLY : constant := (2 ** 7);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:401
   CL_MEM_HOST_READ_ONLY : constant := (2 ** 8);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:402
   CL_MEM_HOST_NO_ACCESS : constant := (2 ** 9);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:403
   CL_MEM_SVM_FINE_GRAIN_BUFFER : constant := (2 ** 10);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:404
   CL_MEM_SVM_ATOMICS : constant := (2 ** 11);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:405
   CL_MEM_KERNEL_READ_AND_WRITE : constant := (2 ** 12);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:406

   CL_MIGRATE_MEM_OBJECT_HOST : constant := (2 ** 0);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:409
   CL_MIGRATE_MEM_OBJECT_CONTENT_UNDEFINED : constant := (2 ** 1);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:410

   CL_R : constant := 16#10B0#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:413
   CL_A : constant := 16#10B1#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:414
   CL_RG : constant := 16#10B2#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:415
   CL_RA : constant := 16#10B3#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:416
   CL_RGB : constant := 16#10B4#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:417
   CL_RGBA : constant := 16#10B5#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:418
   CL_BGRA : constant := 16#10B6#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:419
   CL_ARGB : constant := 16#10B7#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:420
   CL_INTENSITY : constant := 16#10B8#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:421
   CL_LUMINANCE : constant := 16#10B9#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:422
   CL_Rx : constant := 16#10BA#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:423
   CL_RGx : constant := 16#10BB#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:424
   CL_RGBx : constant := 16#10BC#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:425
   CL_DEPTH : constant := 16#10BD#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:426
   CL_DEPTH_STENCIL : constant := 16#10BE#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:427
   CL_sRGB : constant := 16#10BF#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:428
   CL_sRGBx : constant := 16#10C0#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:429
   CL_sRGBA : constant := 16#10C1#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:430
   CL_sBGRA : constant := 16#10C2#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:431
   CL_ABGR : constant := 16#10C3#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:432

   CL_SNORM_INT8 : constant := 16#10D0#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:435
   CL_SNORM_INT16 : constant := 16#10D1#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:436
   CL_UNORM_INT8 : constant := 16#10D2#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:437
   CL_UNORM_INT16 : constant := 16#10D3#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:438
   CL_UNORM_SHORT_565 : constant := 16#10D4#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:439
   CL_UNORM_SHORT_555 : constant := 16#10D5#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:440
   CL_UNORM_INT_101010 : constant := 16#10D6#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:441
   CL_SIGNED_INT8 : constant := 16#10D7#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:442
   CL_SIGNED_INT16 : constant := 16#10D8#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:443
   CL_SIGNED_INT32 : constant := 16#10D9#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:444
   CL_UNSIGNED_INT8 : constant := 16#10DA#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:445
   CL_UNSIGNED_INT16 : constant := 16#10DB#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:446
   CL_UNSIGNED_INT32 : constant := 16#10DC#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:447
   CL_HALF_FLOAT : constant := 16#10DD#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:448
   CL_FLOAT : constant := 16#10DE#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:449
   CL_UNORM_INT24 : constant := 16#10DF#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:450
   CL_UNORM_INT_101010_2 : constant := 16#10E0#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:451

   CL_MEM_OBJECT_BUFFER : constant := 16#10F0#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:454
   CL_MEM_OBJECT_IMAGE2D : constant := 16#10F1#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:455
   CL_MEM_OBJECT_IMAGE3D : constant := 16#10F2#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:456
   CL_MEM_OBJECT_IMAGE2D_ARRAY : constant := 16#10F3#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:457
   CL_MEM_OBJECT_IMAGE1D : constant := 16#10F4#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:458
   CL_MEM_OBJECT_IMAGE1D_ARRAY : constant := 16#10F5#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:459
   CL_MEM_OBJECT_IMAGE1D_BUFFER : constant := 16#10F6#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:460
   CL_MEM_OBJECT_PIPE : constant := 16#10F7#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:461

   CL_MEM_TYPE : constant := 16#1100#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:464
   CL_MEM_FLAGS : constant := 16#1101#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:465
   CL_MEM_SIZE : constant := 16#1102#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:466
   CL_MEM_HOST_PTR : constant := 16#1103#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:467
   CL_MEM_MAP_COUNT : constant := 16#1104#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:468
   CL_MEM_REFERENCE_COUNT : constant := 16#1105#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:469
   CL_MEM_CONTEXT : constant := 16#1106#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:470
   CL_MEM_ASSOCIATED_MEMOBJECT : constant := 16#1107#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:471
   CL_MEM_OFFSET : constant := 16#1108#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:472
   CL_MEM_USES_SVM_POINTER : constant := 16#1109#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:473

   CL_IMAGE_FORMAT : constant := 16#1110#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:476
   CL_IMAGE_ELEMENT_SIZE : constant := 16#1111#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:477
   CL_IMAGE_ROW_PITCH : constant := 16#1112#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:478
   CL_IMAGE_SLICE_PITCH : constant := 16#1113#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:479
   CL_IMAGE_WIDTH : constant := 16#1114#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:480
   CL_IMAGE_HEIGHT : constant := 16#1115#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:481
   CL_IMAGE_DEPTH : constant := 16#1116#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:482
   CL_IMAGE_ARRAY_SIZE : constant := 16#1117#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:483
   CL_IMAGE_BUFFER : constant := 16#1118#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:484
   CL_IMAGE_NUM_MIP_LEVELS : constant := 16#1119#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:485
   CL_IMAGE_NUM_SAMPLES : constant := 16#111A#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:486

   CL_PIPE_PACKET_SIZE : constant := 16#1120#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:489
   CL_PIPE_MAX_PACKETS : constant := 16#1121#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:490

   CL_ADDRESS_NONE : constant := 16#1130#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:493
   CL_ADDRESS_CLAMP_TO_EDGE : constant := 16#1131#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:494
   CL_ADDRESS_CLAMP : constant := 16#1132#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:495
   CL_ADDRESS_REPEAT : constant := 16#1133#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:496
   CL_ADDRESS_MIRRORED_REPEAT : constant := 16#1134#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:497

   CL_FILTER_NEAREST : constant := 16#1140#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:500
   CL_FILTER_LINEAR : constant := 16#1141#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:501

   CL_SAMPLER_REFERENCE_COUNT : constant := 16#1150#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:504
   CL_SAMPLER_CONTEXT : constant := 16#1151#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:505
   CL_SAMPLER_NORMALIZED_COORDS : constant := 16#1152#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:506
   CL_SAMPLER_ADDRESSING_MODE : constant := 16#1153#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:507
   CL_SAMPLER_FILTER_MODE : constant := 16#1154#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:508
   CL_SAMPLER_MIP_FILTER_MODE : constant := 16#1155#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:509
   CL_SAMPLER_LOD_MIN : constant := 16#1156#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:510
   CL_SAMPLER_LOD_MAX : constant := 16#1157#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:511

   CL_MAP_READ : constant := (2 ** 0);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:514
   CL_MAP_WRITE : constant := (2 ** 1);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:515
   CL_MAP_WRITE_INVALIDATE_REGION : constant := (2 ** 2);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:516

   CL_PROGRAM_REFERENCE_COUNT : constant := 16#1160#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:519
   CL_PROGRAM_CONTEXT : constant := 16#1161#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:520
   CL_PROGRAM_NUM_DEVICES : constant := 16#1162#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:521
   CL_PROGRAM_DEVICES : constant := 16#1163#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:522
   CL_PROGRAM_SOURCE : constant := 16#1164#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:523
   CL_PROGRAM_BINARY_SIZES : constant := 16#1165#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:524
   CL_PROGRAM_BINARIES : constant := 16#1166#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:525
   CL_PROGRAM_NUM_KERNELS : constant := 16#1167#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:526
   CL_PROGRAM_KERNEL_NAMES : constant := 16#1168#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:527
   CL_PROGRAM_IL : constant := 16#1169#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:528

   CL_PROGRAM_BUILD_STATUS : constant := 16#1181#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:531
   CL_PROGRAM_BUILD_OPTIONS : constant := 16#1182#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:532
   CL_PROGRAM_BUILD_LOG : constant := 16#1183#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:533
   CL_PROGRAM_BINARY_TYPE : constant := 16#1184#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:534
   CL_PROGRAM_BUILD_GLOBAL_VARIABLE_TOTAL_SIZE : constant := 16#1185#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:535

   CL_PROGRAM_BINARY_TYPE_NONE : constant := 16#0#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:538
   CL_PROGRAM_BINARY_TYPE_COMPILED_OBJECT : constant := 16#1#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:539
   CL_PROGRAM_BINARY_TYPE_LIBRARY : constant := 16#2#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:540
   CL_PROGRAM_BINARY_TYPE_EXECUTABLE : constant := 16#4#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:541

   CL_BUILD_SUCCESS : constant := 0;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:544
   CL_BUILD_NONE : constant := -1;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:545
   CL_BUILD_ERROR : constant := -2;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:546
   CL_BUILD_IN_PROGRESS : constant := -3;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:547

   CL_KERNEL_FUNCTION_NAME : constant := 16#1190#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:550
   CL_KERNEL_NUM_ARGS : constant := 16#1191#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:551
   CL_KERNEL_REFERENCE_COUNT : constant := 16#1192#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:552
   CL_KERNEL_CONTEXT : constant := 16#1193#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:553
   CL_KERNEL_PROGRAM : constant := 16#1194#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:554
   CL_KERNEL_ATTRIBUTES : constant := 16#1195#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:555
   CL_KERNEL_MAX_NUM_SUB_GROUPS : constant := 16#11B9#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:556
   CL_KERNEL_COMPILE_NUM_SUB_GROUPS : constant := 16#11BA#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:557

   CL_KERNEL_ARG_ADDRESS_QUALIFIER : constant := 16#1196#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:560
   CL_KERNEL_ARG_ACCESS_QUALIFIER : constant := 16#1197#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:561
   CL_KERNEL_ARG_TYPE_NAME : constant := 16#1198#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:562
   CL_KERNEL_ARG_TYPE_QUALIFIER : constant := 16#1199#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:563
   CL_KERNEL_ARG_NAME : constant := 16#119A#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:564

   CL_KERNEL_ARG_ADDRESS_GLOBAL : constant := 16#119B#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:567
   CL_KERNEL_ARG_ADDRESS_LOCAL : constant := 16#119C#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:568
   CL_KERNEL_ARG_ADDRESS_CONSTANT : constant := 16#119D#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:569
   CL_KERNEL_ARG_ADDRESS_PRIVATE : constant := 16#119E#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:570

   CL_KERNEL_ARG_ACCESS_READ_ONLY : constant := 16#11A0#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:573
   CL_KERNEL_ARG_ACCESS_WRITE_ONLY : constant := 16#11A1#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:574
   CL_KERNEL_ARG_ACCESS_READ_WRITE : constant := 16#11A2#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:575
   CL_KERNEL_ARG_ACCESS_NONE : constant := 16#11A3#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:576

   CL_KERNEL_ARG_TYPE_NONE : constant := 0;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:579
   CL_KERNEL_ARG_TYPE_CONST : constant := (2 ** 0);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:580
   CL_KERNEL_ARG_TYPE_RESTRICT : constant := (2 ** 1);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:581
   CL_KERNEL_ARG_TYPE_VOLATILE : constant := (2 ** 2);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:582
   CL_KERNEL_ARG_TYPE_PIPE : constant := (2 ** 3);  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:583

   CL_KERNEL_WORK_GROUP_SIZE : constant := 16#11B0#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:586
   CL_KERNEL_COMPILE_WORK_GROUP_SIZE : constant := 16#11B1#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:587
   CL_KERNEL_LOCAL_MEM_SIZE : constant := 16#11B2#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:588
   CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE : constant := 16#11B3#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:589
   CL_KERNEL_PRIVATE_MEM_SIZE : constant := 16#11B4#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:590
   CL_KERNEL_GLOBAL_WORK_SIZE : constant := 16#11B5#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:591

   CL_KERNEL_MAX_SUB_GROUP_SIZE_FOR_NDRANGE : constant := 16#2033#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:594
   CL_KERNEL_SUB_GROUP_COUNT_FOR_NDRANGE : constant := 16#2034#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:595
   CL_KERNEL_LOCAL_SIZE_FOR_SUB_GROUP_COUNT : constant := 16#11B8#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:596

   CL_KERNEL_EXEC_INFO_SVM_PTRS : constant := 16#11B6#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:599
   CL_KERNEL_EXEC_INFO_SVM_FINE_GRAIN_SYSTEM : constant := 16#11B7#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:600

   CL_EVENT_COMMAND_QUEUE : constant := 16#11D0#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:603
   CL_EVENT_COMMAND_TYPE : constant := 16#11D1#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:604
   CL_EVENT_REFERENCE_COUNT : constant := 16#11D2#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:605
   CL_EVENT_COMMAND_EXECUTION_STATUS : constant := 16#11D3#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:606
   CL_EVENT_CONTEXT : constant := 16#11D4#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:607

   CL_COMMAND_NDRANGE_KERNEL : constant := 16#11F0#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:610
   CL_COMMAND_TASK : constant := 16#11F1#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:611
   CL_COMMAND_NATIVE_KERNEL : constant := 16#11F2#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:612
   CL_COMMAND_READ_BUFFER : constant := 16#11F3#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:613
   CL_COMMAND_WRITE_BUFFER : constant := 16#11F4#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:614
   CL_COMMAND_COPY_BUFFER : constant := 16#11F5#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:615
   CL_COMMAND_READ_IMAGE : constant := 16#11F6#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:616
   CL_COMMAND_WRITE_IMAGE : constant := 16#11F7#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:617
   CL_COMMAND_COPY_IMAGE : constant := 16#11F8#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:618
   CL_COMMAND_COPY_IMAGE_TO_BUFFER : constant := 16#11F9#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:619
   CL_COMMAND_COPY_BUFFER_TO_IMAGE : constant := 16#11FA#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:620
   CL_COMMAND_MAP_BUFFER : constant := 16#11FB#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:621
   CL_COMMAND_MAP_IMAGE : constant := 16#11FC#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:622
   CL_COMMAND_UNMAP_MEM_OBJECT : constant := 16#11FD#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:623
   CL_COMMAND_MARKER : constant := 16#11FE#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:624
   CL_COMMAND_ACQUIRE_GL_OBJECTS : constant := 16#11FF#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:625
   CL_COMMAND_RELEASE_GL_OBJECTS : constant := 16#1200#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:626
   CL_COMMAND_READ_BUFFER_RECT : constant := 16#1201#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:627
   CL_COMMAND_WRITE_BUFFER_RECT : constant := 16#1202#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:628
   CL_COMMAND_COPY_BUFFER_RECT : constant := 16#1203#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:629
   CL_COMMAND_USER : constant := 16#1204#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:630
   CL_COMMAND_BARRIER : constant := 16#1205#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:631
   CL_COMMAND_MIGRATE_MEM_OBJECTS : constant := 16#1206#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:632
   CL_COMMAND_FILL_BUFFER : constant := 16#1207#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:633
   CL_COMMAND_FILL_IMAGE : constant := 16#1208#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:634
   CL_COMMAND_SVM_FREE : constant := 16#1209#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:635
   CL_COMMAND_SVM_MEMCPY : constant := 16#120A#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:636
   CL_COMMAND_SVM_MEMFILL : constant := 16#120B#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:637
   CL_COMMAND_SVM_MAP : constant := 16#120C#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:638
   CL_COMMAND_SVM_UNMAP : constant := 16#120D#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:639
   CL_COMMAND_MIGRATE_SVM_MEM_OBJECTS : constant := 16#120E#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:640

   CL_COMPLETE : constant := 16#0#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:643
   CL_RUNNING : constant := 16#1#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:644
   CL_SUBMITTED : constant := 16#2#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:645
   CL_QUEUED : constant := 16#3#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:646

   CL_BUFFER_CREATE_TYPE_REGION : constant := 16#1220#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:649

   CL_PROFILING_COMMAND_QUEUED : constant := 16#1280#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:652
   CL_PROFILING_COMMAND_SUBMIT : constant := 16#1281#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:653
   CL_PROFILING_COMMAND_START : constant := 16#1282#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:654
   CL_PROFILING_COMMAND_END : constant := 16#1283#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:655
   CL_PROFILING_COMMAND_COMPLETE : constant := 16#1284#;  --  C:\Intel\OpenCL\sdk\include\CL\cl.h:656

end cl_h;
