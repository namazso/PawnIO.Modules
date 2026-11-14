//  This Source Code Form is subject to the terms of the Mozilla Public
//  License, v. 2.0. If a copy of the MPL was not distributed with this
//  file, You can obtain one at http://mozilla.org/MPL/2.0/.
#pragma once
#include <cstdint>
#include <cassert>
#include <cstdlib>
#include <iterator>
#include <limits>
#include <type_traits>
#include <cstddef>
#include <initializer_list>
#ifndef AMX_ASSERT
#ifdef NDEBUG
#define AMX_ASSERT(cond) ((cond) ? (void)0 : std::abort())
#else
#define AMX_ASSERT(cond) assert(cond)
#endif
#endif

namespace amx
{
  enum class error
  {
    success = 0,
    access_violation,
    access_violation_code,
    invalid_instruction,
    invalid_operand,
    division_with_zero,
    halt,
    bounds,
    callback_abort,
    stack_error,
    heap_error
  };

  namespace detail
  {
    template <typename A, typename B>
    class max
    {
      static_assert(std::numeric_limits<A>::radix == std::numeric_limits<B>::radix, "radix mismatch");

      constexpr static auto a_digits = std::numeric_limits<A>::digits;
      constexpr static auto b_digits = std::numeric_limits<B>::digits;

    public:
      using type = typename std::conditional<(a_digits > b_digits), A, B>::type;
    };

    template <typename A, typename B>
    using max_t = typename max<A, B>::type;

    template <typename A, typename B>
    class min
    {
      static_assert(std::numeric_limits<A>::radix == std::numeric_limits<B>::radix, "radix mismatch");

      constexpr static auto a_digits = std::numeric_limits<A>::digits;
      constexpr static auto b_digits = std::numeric_limits<B>::digits;

    public:
      using type = typename std::conditional<(a_digits < b_digits), A, B>::type;
    };

    template <typename A, typename B>
    using min_t = typename min<A, B>::type;

    template <typename Cell>
    struct cell_traits
    {
      using ucell = typename std::make_unsigned<Cell>::type;
      using scell = typename std::make_signed<Cell>::type;
      using cell = ucell;

      using ushorter = min_t<ucell, uintptr_t>;
      using sshorter = min_t<scell, intptr_t>;
      using shorter = ushorter;

      using ulonger = max_t<ucell, uintptr_t>;
      using slonger = max_t<scell, intptr_t>;
      using longer = ulonger;

      constexpr static shorter cell_bytes = (shorter)(sizeof(cell));
      constexpr static shorter cell_bits = (shorter)(std::numeric_limits<cell>::digits);

      constexpr static shorter misalign_mask = cell_bytes - 1;
      constexpr static longer align_mask = ~(longer)misalign_mask;

      static_assert(cell_bytes * 8 == cell_bits, "only 8 bit bytes are supported");
      static_assert(cell_bytes == 1 || cell_bytes == 2 || cell_bytes == 4 || cell_bytes == 8, "");

#define DEFINE_CELL_MEMBERS(Cell) \
  using ucell = typename ::amx::detail::cell_traits<Cell>::ucell;\
  using scell = typename ::amx::detail::cell_traits<Cell>::scell;\
  using cell = typename ::amx::detail::cell_traits<Cell>::cell;\
  using ushorter = typename ::amx::detail::cell_traits<Cell>::ushorter;\
  using sshorter = typename ::amx::detail::cell_traits<Cell>::sshorter;\
  using shorter = typename ::amx::detail::cell_traits<Cell>::shorter;\
  using ulonger = typename ::amx::detail::cell_traits<Cell>::ulonger;\
  using slonger = typename ::amx::detail::cell_traits<Cell>::slonger;\
  using longer = typename ::amx::detail::cell_traits<Cell>::longer;\
  constexpr static auto cell_bytes = ::amx::detail::cell_traits<Cell>::cell_bytes;\
  constexpr static auto cell_bits = ::amx::detail::cell_traits<Cell>::cell_bits;\
  constexpr static auto misalign_mask = ::amx::detail::cell_traits<Cell>::misalign_mask;\
  constexpr static auto align_mask = ::amx::detail::cell_traits<Cell>::align_mask

    };

    template <typename Cell, size_t IndexBits>
    class memory_backing_paged_buffers
    {
      DEFINE_CELL_MEMBERS(Cell);

      constexpr static size_t index_bits = IndexBits;
      constexpr static size_t offset_bits = cell_bits - index_bits;

      static_assert(index_bits < cell_bits, "page bits must be less than cell bit size!");
      static_assert(index_bits >= 1, "must use at least 1 bit for index");

      constexpr static auto page_count = (cell)((ulonger)1 << index_bits);
      constexpr static auto page_size = (cell)((ulonger)1 << offset_bits);

      struct mapping
      {
        cell* buf;
        size_t size;
      };

      mapping _mappings[page_count]{};

      static constexpr cell page_index(cell va) { return va >> offset_bits; }
      static constexpr cell page_offset(cell va) { return va & ((cell)((cell)~(cell)0 << index_bits) >> index_bits); }
      static constexpr cell make_va(cell index, cell offset) { return (cell)((cell)(index << offset_bits) | offset); }
      mapping* mapping_for_va(cell va) { return &_mappings[page_index(va)]; }
      const mapping* mapping_for_va(cell va) const { return &_mappings[page_index(va)]; }

    public:
      cell* translate(cell va) const
      {
        if (va % cell_bytes != 0)
          return nullptr;

        const auto m = mapping_for_va(va);
        if (!m->buf)
          return nullptr;
        const auto off = page_offset(va);
        if (off >= m->size)
          return nullptr;
        return m->buf + off / cell_bytes;
      }

      bool map(cell* buf, size_t size, cell& va)
      {
        if (size == 0)
        {
          va = ~(cell)misalign_mask; // highest valid address
          return true;
        }

        if ((longer)size > (longer)((cell)~(cell)0 / cell_bytes))
          return false; // mapping bigger than address space
        if (size > (size_t)-1 / (size_t)cell_bytes)
          return false;

        size *= cell_bytes;
        const auto count = (cell)(size / page_size + (size % page_size != 0 ? 1 : 0));
        cell in_a_row = 0;
        cell index = 0;
        do
        {
          if (_mappings[index].buf)
            in_a_row = 0;
          else
            ++in_a_row;

          if (in_a_row == count)
            goto found;

          ++index;
        } while (index != 0 && index != page_count);

        return false;

      found:
        index = (cell)(index - count + 1);
        for (cell i = 0; i < count; ++i)
        {
          _mappings[index + i].buf = buf + page_size / (cell)cell_bytes * i;
          _mappings[index + i].size = size - (size_t)(page_size * i);
        }

        va = make_va(index, 0);
        return true;
      }

      void unmap(cell va, size_t size)
      {
        const auto m = mapping_for_va(va);
        size *= cell_bytes;
        const auto count = (cell)(size / page_size + (size % page_size != 0 ? 1 : 0));
        for (cell i = 0; i < count; ++i)
        {
          m[i].buf = nullptr;
          m[i].size = 0;
        }
      }
    };

    template <typename Cell>
    class memory_backing_contignous_buffer
    {
      DEFINE_CELL_MEMBERS(Cell);

      cell* _buf{};
      size_t _size{};

    public:
      cell* translate(cell va)
      {
        if (va % cell_bytes != 0)
          return nullptr;
        if (va >= _size)
          return nullptr;
        return _buf + (va / cell_bytes);
      }

      bool map(cell* buf, size_t size, cell& va)
      {
        if (_buf)
          return false;
        if ((longer)size > (longer)((cell)~(cell)0 / cell_bytes))
          return false;
        if (size > (size_t)-1 / (size_t)cell_bytes)
          return false;
        _buf = buf;
        _size = size * cell_bytes;
        va = 0;
        return true;
      }

      void unmap(cell va, size_t size)
      {
        AMX_ASSERT(va == 0);
        AMX_ASSERT(size * cell_bytes == _size);
        _buf = 0;
        _size = 0;
      }
    };

    template <typename Cell, size_t ValidBits>
    class memory_backing_partial_address_space
    {
      DEFINE_CELL_MEMBERS(Cell);

      constexpr static size_t valid_bits = ValidBits;
      constexpr static size_t invalid_bits = cell_bits - valid_bits;
      constexpr static cell offset_mask = (cell)((cell)~(cell)0 >> invalid_bits);
      constexpr static cell offset_mask_align = offset_mask & ~(cell)misalign_mask;

      uintptr_t _backing_bits{};

      static_assert(valid_bits <= std::numeric_limits<uintptr_t>::digits, "virtual address space bigger than host");
      static_assert(offset_mask_align != 0, "too few valid bits!");

    public:
      cell* translate(cell va)
      {
        if (!_backing_bits)
          return nullptr;
        if (va % cell_bytes != 0)
          return nullptr;
        return (cell*)((uintptr_t)(va & offset_mask_align) | _backing_bits);
      }

      bool map(cell* buf, size_t size, cell& va)
      {
        va = 0;

        if (_backing_bits)
          return false;

        if (!buf)
          return false;
        if (((uintptr_t)buf & (uintptr_t)offset_mask) != 0)
          return false;
        if (size < ((size_t)offset_mask + 1) / (size_t)cell_bytes)
          return false;

        _backing_bits = (uintptr_t)buf & ~(uintptr_t)offset_mask;
        return true;
      }

      void unmap(cell va, size_t)
      {
        AMX_ASSERT(va == 0);
        _backing_bits = 0;
      }
    };

    template <typename CodeBacking, typename DataBacking>
    class memory_manager_harvard
    {
      CodeBacking _code;
      DataBacking _data;

    public:
      CodeBacking& code() { return _code; }
      DataBacking& data() { return _data; }
    };

    template <typename Backing>
    class memory_manager_neumann
    {
      Backing _backing;

    public:
      Backing& code() { return _backing; }
      Backing& data() { return _backing; }
    };
  }

  template <size_t IndexBits>
  struct memory_backing_paged_buffers
  {
    template <typename Cell>
    using type = detail::memory_backing_paged_buffers<Cell, IndexBits>;
  };

  struct memory_backing_contignous_buffer
  {
    template <typename Cell>
    using type = detail::memory_backing_contignous_buffer<Cell>;
  };

  template <size_t ValidBits>
  struct memory_backing_partial_address_space
  {
    template <typename Cell>
    using type = detail::memory_backing_partial_address_space<Cell, ValidBits>;
  };

  template <typename CodeBacking, typename DataBacking>
  struct memory_manager_harvard
  {
    template <typename Cell>
    using type = detail::memory_manager_harvard<
      typename CodeBacking::template type<Cell>,
      typename DataBacking::template type<Cell>
    >;
  };

  template <typename Backing>
  struct memory_manager_neumann
  {
    template <typename Cell>
    using type = detail::memory_manager_neumann<typename Backing::template type<Cell>>;
  };

  template <typename Cell, typename MemoryManager>
  class amx
  {
  public:
    constexpr static uint32_t version = 11;

    DEFINE_CELL_MEMBERS(Cell);

    using memory_manager_t = typename MemoryManager::template type<cell>;

    memory_manager_t mem{};

    cell* data_v2p(cell v) { return mem.data().translate(DAT + v); }
    cell* code_v2p(cell v) { return mem.code().translate(COD + v); }

  //private:
    // primary register (ALU, general purpose).
    cell PRI{};
    // alternate register (general purpose).
    cell ALT{};
    // stack frame pointer; stack-relative memory reads & writes are relative to the address in this register.
    cell FRM{};
    // code instruction pointer.
    cell CIP{};
    // offset to the start of the data. (data segment base)
    cell DAT{};
    // offset to the start of the code. (code segment base)
    cell COD{};
    // stack top.
    cell STP{};
    // stack index, indicates the current position in the stack. The stack runs downwards from the STP register towards zero.
    cell STK{};
    // heap pointer. Dynamically allocated memory comes from the heapand the HEA register indicates the top of the heap.
    cell HEA{};

  private:
    error step();

  public:
    using callback_t = error(*)(amx* _this, void* user_data, cell index, cell stk, cell& pri);
    enum : cell
    {
      cbid_single_step = (cell)(scell)-1,
      cbid_break = (cell)(scell)-2
    };

  private:
    callback_t _callback{};
    void* _callback_user_data{};

    error fire_callback(cell index)
    {
      if (!_callback)
        return index == cbid_single_step || index == cbid_break ? error::success : error::callback_abort;
      // callback shouldnt modify these after return
      const auto alt = ALT;
      const auto frm = FRM;
      const auto cip = CIP;
      const auto stp = STP;
      const auto stk = STK;
      const auto result = _callback(this, _callback_user_data, index, STK, PRI);
      ALT = alt;
      FRM = frm;
      CIP = cip;
      STP = stp;
      STK = stk;
      return result;
    }

  public:
    error push(cell v)
    {
      if (STK <= HEA)
        return error::stack_error;
      STK -= cell_bytes;
      const auto target = data_v2p(STK);
      if (!target)
        return error::access_violation;
      *target = v;
      return error::success;
    }

    error pop(cell& v)
    {
      const auto target = data_v2p(STK);
      if (!target)
        return error::access_violation;
      v = *target;
      STK += cell_bytes;
      return error::success;
    }

  private:
    error call_raw(cell cip, cell& pri)
    {
      // As of version 2.0, the PAWN compiler puts a HALT opcode at the start of the code (so at code address 0). Before
      // jumping to the entry point (a function), the abstract machine pushes a zero return address onto the stack. When
      // the entry point returns, it returns to the zero address and sees the HALT instruction.
      constexpr auto invalid_cip = (cell)0;
      auto result = push(invalid_cip);
      CIP = cip;
      while (result == error::success && CIP != invalid_cip)
      {
        result = fire_callback(cbid_single_step);
        if (result != error::success)
          break;
        result = step();
      }
      pri = PRI;
      return result;
    }

  public:
    error call(cell cip, cell& pri, std::initializer_list<cell> args = {})
    {
      const auto stk = STK;
      const auto frm = FRM;
      const auto hea = HEA;

      auto result = error::success;
      cell size{};
      for (auto it = std::crbegin(args); it != std::crend(args); ++it)
      {
        result = push(*it);
        if (result != error::success)
          break;
        size += cell_bytes;
      }

      if (result == error::success)
        result = push(size);

      if (result == error::success)
        result = call_raw(cip, pri);

      if (result != error::success)
      {
        STK = stk;
        FRM = frm;
        HEA = hea;
      }

      return result;
    }

    amx(
      callback_t callback,
      void* callback_user
    )
      : _callback(callback)
      , _callback_user_data(callback_user)
    {}

    constexpr amx() = default;

    void init(
      callback_t callback,
      void* callback_user
    )
    {
      _callback = callback;
      _callback_user_data = callback_user;
    }

    ~amx() = default;

    amx(const amx&) = delete;
    amx(amx&&) = delete;

    amx& operator=(const amx&) = delete;
    amx& operator=(amx&&) = delete;
  };

  template <typename Cell, typename MemoryManager>
  error amx<Cell, MemoryManager>::step()
  {

    enum : cell {
      OP_NOP = 0,
      OP_LOAD_PRI,
      OP_LOAD_ALT,
      OP_LOAD_S_PRI,
      OP_LOAD_S_ALT,
      OP_LREF_S_PRI,
      OP_LREF_S_ALT,
      OP_LOAD_I,
      OP_LODB_I,
      OP_CONST_PRI,
      OP_CONST_ALT,
      OP_ADDR_PRI,
      OP_ADDR_ALT,
      OP_STOR,
      OP_STOR_S,
      OP_SREF_S,
      OP_STOR_I,
      OP_STRB_I,
      OP_ALIGN_PRI,
      OP_LCTRL,
      OP_SCTRL,
      OP_XCHG,
      OP_PUSH_PRI,
      OP_PUSH_ALT,
      OP_PUSHR_PRI,
      OP_POP_PRI,
      OP_POP_ALT,
      OP_PICK,
      OP_STACK,
      OP_HEAP,
      OP_PROC,
      OP_RET,
      OP_RETN,
      OP_CALL,
      OP_JUMP,
      OP_JZER,
      OP_JNZ,
      OP_SHL,
      OP_SHR,
      OP_SSHR,
      OP_SHL_C_PRI,
      OP_SHL_C_ALT,
      OP_SMUL,
      OP_SDIV,
      OP_ADD,
      OP_SUB,
      OP_AND,
      OP_OR,
      OP_XOR,
      OP_NOT,
      OP_NEG,
      OP_INVERT,
      OP_EQ,
      OP_NEQ,
      OP_SLESS,
      OP_SLEQ,
      OP_SGRTR,
      OP_SGEQ,
      OP_INC_PRI,
      OP_INC_ALT,
      OP_INC_I,
      OP_DEC_PRI,
      OP_DEC_ALT,
      OP_DEC_I,
      OP_MOVS,
      OP_CMPS,
      OP_FILL,
      OP_HALT,
      OP_BOUNDS,
      OP_SYSREQ,
      OP_SWITCH,
      OP_SWAP_PRI,
      OP_SWAP_ALT,
      OP_BREAK,
      OP_CASETBL,
      /* ----- */
      OP_NUM_OPCODES
    };

    cell* _tmp{};

    const static auto INC = [](cell& v) -> cell& { return (v += cell_bytes); };
    const static auto DEC = [](cell& v) -> cell& { return (v -= cell_bytes); };
    const static auto INCP = [](cell& v) -> cell { cell c = v; return (v += cell_bytes), c; };

    _tmp = code_v2p(INCP(CIP));
    if (!_tmp)
      return error::access_violation_code;
    cell opcode{ *_tmp };
    cell operand{};

#define OPERAND() do { _tmp = code_v2p(INCP(CIP)); if(!_tmp) return error::access_violation_code; operand = *_tmp; } while(0)

#define DATA(v) do { _tmp = data_v2p(v); if(!_tmp) return error::access_violation; } while(0)
#define CODEDATA(v) do { _tmp = code_v2p(v); if(!_tmp) return error::access_violation; } while(0)
#define RESULT() (*_tmp)

#define PUSH(v) do {\
    if (STK <= HEA) return error::stack_error;\
    DEC(STK);\
    cell _push_tmp1 = v;\
    cell* _push_tmp2 = data_v2p(STK);\
    if(!_push_tmp2) return error::access_violation;\
    *_push_tmp2 = _push_tmp1;\
  } while(0)

#define POP(v) do {\
    cell& _pop_tmp1 = v;\
    cell* _pop_tmp2 = data_v2p(STK);\
    if(!_pop_tmp2) return error::access_violation;\
    _pop_tmp1 = *_pop_tmp2;\
    INC(STK);\
  } while(0)

    switch (opcode)
    {
    case OP_NOP:
      break;

    case OP_LOAD_PRI:
      OPERAND();
      DATA(operand);
      PRI = RESULT();
      break;
    case OP_LOAD_ALT:
      OPERAND();
      DATA(operand);
      ALT = RESULT();
      break;

    case OP_LOAD_S_PRI:
      OPERAND();
      DATA(FRM + operand);
      PRI = RESULT();
      break;
    case OP_LOAD_S_ALT:
      OPERAND();
      DATA(FRM + operand);
      ALT = RESULT();
      break;

    case OP_LREF_S_PRI:
      OPERAND();
      DATA(FRM + operand);
      DATA(RESULT());
      PRI = RESULT();
      break;
    case OP_LREF_S_ALT:
      OPERAND();
      DATA(FRM + operand);
      DATA(RESULT());
      ALT = RESULT();
      break;

    case OP_LOAD_I:
      DATA(PRI);
      PRI = RESULT();
      break;

    case OP_LODB_I:
      OPERAND();
      if ((operand == 1 || operand == 2 || operand == 4) && operand <= (cell)cell_bytes)
      {
        cell value{};
        for (cell i = 0; i < operand; ++i)
        {
          const auto address = (cell)(PRI + i);
          DATA((cell)(address & ~(cell)misalign_mask));
          value |= (cell)(((RESULT() >> ((address & (cell)misalign_mask) * 8)) & 0xFF) << (i * 8));
        }
        PRI = value;
      }
      break;

    case OP_CONST_PRI:
      OPERAND();
      PRI = operand;
      break;
    case OP_CONST_ALT:
      OPERAND();
      ALT = operand;
      break;

    case OP_ADDR_PRI:
      OPERAND();
      PRI = FRM + operand;
      break;
    case OP_ADDR_ALT:
      OPERAND();
      ALT = FRM + operand;
      break;

    case OP_STOR:
      OPERAND();
      DATA(operand);
      RESULT() = PRI;
      break;

    case OP_STOR_S:
      OPERAND();
      DATA(FRM + operand);
      RESULT() = PRI;
      break;

    case OP_SREF_S:
      OPERAND();
      DATA(FRM + operand);
      DATA(RESULT());
      RESULT() = PRI;
      break;

    case OP_STOR_I:
      DATA(ALT);
      RESULT() = PRI;
      break;

    case OP_STRB_I:
      OPERAND();
      if ((operand == 1 || operand == 2 || operand == 4) && operand <= (cell)cell_bytes)
      {
        for (cell i = 0; i < operand; ++i)
        {
          const auto address = (cell)(ALT + i);
          DATA((cell)(address & ~(cell)misalign_mask));
          const auto shift = (cell)((address & (cell)misalign_mask) * 8);
          RESULT() = (cell)((RESULT() & ~((cell)0xFF << shift)) | (((PRI >> (i * 8)) & 0xFF) << shift));
        }
      }
      break;

    case OP_ALIGN_PRI:
      OPERAND();
      if (operand < cell_bytes)
        PRI ^= (cell)(cell_bytes - operand);
      break;

    case OP_LCTRL:
      OPERAND();
      switch (operand)
      {
      case 0:
        PRI = COD;
        break;
      case 1:
        PRI = DAT;
        break;
      case 2:
        PRI = HEA;
        break;
      case 3:
        PRI = STP;
        break;
      case 4:
        PRI = STK;
        break;
      case 5:
        PRI = FRM;
        break;
      case 6:
        PRI = CIP;
        break;
      default:
        return error::invalid_operand;
      }
      break;

    case OP_SCTRL:
      OPERAND();
      switch (operand)
      {
      case 2:
        HEA = PRI;
        if (HEA > STK)
          return error::heap_error;
        break;
      case 4:
        STK = PRI;
        if (STK < HEA || STK > STP)
          return error::stack_error;
        break;
      case 5:
        FRM = PRI;
        break;
      case 6:
        CIP = PRI;
        break;
      default:
        return error::invalid_operand;
      }
      break;

    case OP_XCHG:
      operand = ALT;
      ALT = PRI;
      PRI = operand;
      break;

    case OP_PUSH_PRI:
      PUSH(PRI);
      break;
    case OP_PUSH_ALT:
      PUSH(ALT);
      break;

    case OP_PUSHR_PRI:
      PUSH(PRI + DAT);
      break;

    case OP_POP_PRI:
      POP(PRI);
      break;
    case OP_POP_ALT:
      POP(ALT);
      break;

    case OP_PICK:
      OPERAND();
      DATA(STK + operand);
      PRI = RESULT();
      break;

    case OP_STACK:
      OPERAND();
      STK += operand;
      ALT = STK;
      if (STK < HEA || STK > STP)
        return error::stack_error;
      break;

    case OP_HEAP:
      OPERAND();
      ALT = HEA;
      HEA += operand;
      if (HEA > STK)
        return error::heap_error;
      break;

    case OP_PROC:
      PUSH(FRM);
      FRM = STK;
      break;

    case OP_RET:
      POP(FRM);
      POP(CIP);
      break;

    case OP_RETN:
      POP(FRM);
      POP(CIP);
      DATA(STK);
      STK += RESULT() + cell_bytes;
      if (STK < HEA || STK > STP)
        return error::stack_error;
      break;

    case OP_CALL:
      OPERAND();
      PUSH(CIP);
      CIP = CIP - 2 * cell_bytes + operand;
      break;

    case OP_JUMP:
      OPERAND();
      CIP = CIP - 2 * cell_bytes + operand;
      break;

    case OP_JZER:
      OPERAND();
      if (PRI == 0)
        CIP = CIP - 2 * cell_bytes + operand;
      break;

    case OP_JNZ:
      OPERAND();
      if (PRI != 0)
        CIP = CIP - 2 * cell_bytes + operand;
      break;

    case OP_SHL:
      PRI = (cell)(ALT >= (cell)cell_bits ? 0 : PRI << ALT);
      break;

    case OP_SHR:
      PRI = (cell)(ALT >= (cell)cell_bits ? 0 : PRI >> ALT);
      break;

    case OP_SSHR:
      if (ALT >= (cell)cell_bits)
        PRI = (scell)PRI < 0 ? (cell)~(cell)0 : (cell)0;
      else
        PRI = (cell)((scell)PRI >> ALT); // not implementation defined since C++20.
      break;

    case OP_SHL_C_PRI:
      OPERAND();
      PRI = (cell)(operand >= (cell)cell_bits ? 0 : PRI << operand);
      break;

    case OP_SHL_C_ALT:
      OPERAND();
      ALT = (cell)(operand >= (cell)cell_bits ? 0 : ALT << operand);
      break;

    case OP_SMUL:
      PRI = (cell)((ulonger)PRI * (ulonger)ALT);
      break;

    case OP_SDIV:
      if (PRI == 0)
        return error::division_with_zero;
      operand = PRI;
      if (operand == (cell)~(cell)0)
      {
        PRI = (cell)(0 - ALT);
        ALT = 0;
        break;
      }
      PRI = (cell)((scell)ALT / (scell)operand);
      ALT = (cell)((scell)ALT % (scell)operand);
      if (ALT != 0 && (scell)(ALT ^ operand) < 0) {
        --PRI;
        ALT += operand;
      }
      break;

    case OP_ADD:
      PRI += ALT;
      break;

    case OP_SUB:
      PRI = ALT - PRI;
      break;

    case OP_AND:
      PRI &= ALT;
      break;

    case OP_OR:
      PRI |= ALT;
      break;

    case OP_XOR:
      PRI ^= ALT;
      break;

    case OP_NOT:
      PRI = !PRI;
      break;

    case OP_NEG:
      PRI = (cell)(0 - PRI);
      break;

    case OP_INVERT:
      PRI = ~PRI;
      break;

    case OP_EQ:
      PRI = (cell)(PRI == ALT);
      break;

    case OP_NEQ:
      PRI = (cell)(PRI != ALT);
      break;

    case OP_SLESS:
      PRI = (cell)((scell)PRI < (scell)ALT);
      break;

    case OP_SLEQ:
      PRI = (cell)((scell)PRI <= (scell)ALT);
      break;

    case OP_SGRTR:
      PRI = (cell)((scell)PRI > (scell)ALT);
      break;

    case OP_SGEQ:
      PRI = (cell)((scell)PRI >= (scell)ALT);
      break;

    case OP_INC_PRI:
      ++PRI;
      break;
    case OP_INC_ALT:
      ++ALT;
      break;

    case OP_INC_I:
      DATA(PRI);
      ++RESULT();
      break;

    case OP_DEC_PRI:
      --PRI;
      break;
    case OP_DEC_ALT:
      --ALT;
      break;

    case OP_DEC_I:
      DATA(PRI);
      --RESULT();
      break;

    case OP_MOVS:
      OPERAND();
      for (cell i = 0; i < operand; ++i)
      {
        const auto src = (cell)(PRI + i);
        DATA((cell)(src & ~(cell)misalign_mask));
        const auto value = (cell)((RESULT() >> ((src & (cell)misalign_mask) * 8)) & 0xFF);
        const auto dst = (cell)(ALT + i);
        DATA((cell)(dst & ~(cell)misalign_mask));
        const auto shift = (cell)((dst & (cell)misalign_mask) * 8);
        RESULT() = (cell)((RESULT() & ~((cell)0xFF << shift)) | (value << shift));
      }
      break;

    case OP_CMPS:
      OPERAND();
      {
        cell diff = 0;
        for (cell i = 0; diff == 0 && i < operand; ++i)
        {
          const auto lhs = (cell)(PRI + i);
          DATA((cell)(lhs & ~(cell)misalign_mask));
          const auto lhs_value = (cell)((RESULT() >> ((lhs & (cell)misalign_mask) * 8)) & 0xFF);
          const auto rhs = (cell)(ALT + i);
          DATA((cell)(rhs & ~(cell)misalign_mask));
          diff = (cell)(lhs_value - ((RESULT() >> ((rhs & (cell)misalign_mask) * 8)) & 0xFF));
        }
        PRI = diff;
        break;
      }

    case OP_FILL:
      OPERAND();
      if (operand % cell_bytes != 0)
        return error::invalid_operand;
      for (cell i = 0; i < operand; i += cell_bytes)
      {
        DATA(ALT + i);
        RESULT() = PRI;
      }
      break;

    case OP_HALT:
      OPERAND();
      return error::halt;

    case OP_BOUNDS:
      OPERAND();
      if ((scell)PRI < 0 || (scell)PRI > (scell)operand)
        return error::bounds;
      break;

    case OP_SYSREQ:
      OPERAND();
      {
        const auto result = fire_callback(operand);
        if (result != error::success)
          return result;
        break;
      }

    case OP_SWITCH:
      OPERAND();
      {
        cell casetbl = CIP - 2 * cell_bytes + operand;
        CODEDATA(INCP(casetbl));
        if (RESULT() != OP_CASETBL)
          return error::invalid_operand;
        CODEDATA(INCP(casetbl));
        cell record_count = RESULT();
        CODEDATA(INCP(casetbl));
        CIP = casetbl - cell_bytes * 2 + RESULT(); // no match cip
        while (record_count)
        {
          CODEDATA(INCP(casetbl));
          const cell test_val = RESULT();
          CODEDATA(INCP(casetbl));
          cell match_cip = RESULT();
          if (PRI == test_val)
          {
            CIP = casetbl - cell_bytes * 2 + match_cip;
            break;
          }
          --record_count;
        }
        break;
      }

    case OP_SWAP_PRI:
      DATA(STK);
      operand = RESULT();
      RESULT() = PRI;
      PRI = operand;
      break;

    case OP_SWAP_ALT:
      DATA(STK);
      operand = RESULT();
      RESULT() = ALT;
      ALT = operand;
      break;

    case OP_BREAK:
    {
      const auto result = fire_callback(cbid_break);
      if (result != error::success)
        return result;
      break;
    }

    default:
      return error::invalid_instruction;
    }

#undef DATA
#undef CODEDATA
#undef RESULT
#undef OPERAND
#undef PUSH
#undef POP

    return error::success;
  }
}
