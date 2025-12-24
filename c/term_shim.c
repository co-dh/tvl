/*
 * termbox2 FFI shim for Lean 4
 */
#include <lean/lean.h>
#include <termbox2.h>

// tb_init() -> Int32
lean_obj_res lean_tb_init(lean_obj_arg world) {
    int r = tb_init();
    return lean_io_result_mk_ok(lean_box((uint32_t)(int32_t)r));
}

// tb_shutdown() -> Unit
lean_obj_res lean_tb_shutdown(lean_obj_arg world) {
    tb_shutdown();
    return lean_io_result_mk_ok(lean_box(0));
}

// tb_width() -> UInt32
lean_obj_res lean_tb_width(lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)tb_width()));
}

// tb_height() -> UInt32
lean_obj_res lean_tb_height(lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)tb_height()));
}

// tb_clear() -> Unit
lean_obj_res lean_tb_clear(lean_obj_arg world) {
    tb_clear();
    return lean_io_result_mk_ok(lean_box(0));
}

// tb_present() -> Unit
lean_obj_res lean_tb_present(lean_obj_arg world) {
    tb_present();
    return lean_io_result_mk_ok(lean_box(0));
}

// tb_set_cell(x, y, ch, fg, bg) -> Unit
lean_obj_res lean_tb_set_cell(uint32_t x, uint32_t y, uint32_t ch,
                               uint32_t fg, uint32_t bg, lean_obj_arg world) {
    tb_set_cell((int)x, (int)y, ch, fg, bg);
    return lean_io_result_mk_ok(lean_box(0));
}

// tb_poll_event() -> Event
lean_obj_res lean_tb_poll_event(lean_obj_arg world) {
    struct tb_event ev;
    tb_poll_event(&ev);

    // Create Event structure: type, mod, key, ch, w, h
    lean_object* obj = lean_alloc_ctor(0, 0, 6 * sizeof(uint32_t));
    lean_ctor_set_uint8(obj, 0, ev.type);
    lean_ctor_set_uint8(obj, 1, ev.mod);
    lean_ctor_set_uint16(obj, 2, ev.key);
    lean_ctor_set_uint32(obj, 4, ev.ch);
    lean_ctor_set_uint32(obj, 8, (uint32_t)ev.w);
    lean_ctor_set_uint32(obj, 12, (uint32_t)ev.h);

    return lean_io_result_mk_ok(obj);
}
