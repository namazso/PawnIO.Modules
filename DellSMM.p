//  PawnIO Modules - Modules for various hardware to be used with PawnIO.
//  Copyright (C) 2026  namazso <admin@namazso.eu>
//
//  This library is free software; you can redistribute it and/or
//  modify it under the terms of the GNU Lesser General Public
//  License as published by the Free Software Foundation; either
//  version 2.1 of the License, or (at your option) any later version.
//
//  This library is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public
//  License along with this library; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//
//  SPDX-License-Identifier: LGPL-2.1-or-later

#include <pawnio.inc>

// Signature requests, as used by the i8k / dell-smm-hwmon interface
#define SMM_GET_DELL_SIG1   0xfea3
#define SMM_GET_DELL_SIG2   0xffa3

// A Dell BIOS answers a signature request with 'DIAG' in eax and 'DELL' in edx
#define SMM_SIG_DIAG        0x44494147
#define SMM_SIG_DELL        0x44454c4c

/// Perform an SMM call on CPU 0.
///
/// The interface is only defined on CPU 0; elsewhere it may hang or hand back
/// results that can't be used.
///
/// @param in Input registers in order eax ecx edx ebx esi edi
/// @param out Output registers in order eax ecx edx ebx esi edi
/// @return An NTSTATUS
NTSTATUS:dell_smm_call(const in[6], out[6]) {
    new old_affinity[2];
    new NTSTATUS:status = cpu_set_affinity(0, old_affinity);
    if (!NT_SUCCESS(status))
        return status;

    new bool:ok = query_dell_smm(in, out);

    // Report a failed restore over a failed call, leaving the thread pinned is
    // the worse outcome of the two.
    status = cpu_restore_affinity(old_affinity);
    if (!NT_SUCCESS(status))
        return status;

    return ok ? STATUS_SUCCESS : STATUS_UNSUCCESSFUL;
}

/// Check whether the firmware identifies itself as Dell's.
///
/// @param req_fn Signature request to issue
/// @return Whether the expected signature came back
bool:dell_smm_has_signature(req_fn) {
    new in[6];
    new out[6];

    in[0] = req_fn; // eax

    if (!NT_SUCCESS(dell_smm_call(in, out)))
        return false;

    return (out[0] & 0xFFFFFFFF) == SMM_SIG_DIAG   // eax
        && (out[2] & 0xFFFFFFFF) == SMM_SIG_DELL;  // edx
}

/// Query DELL SMM.
///
/// @param in Input registers in order eax ecx edx ebx esi edi
/// @param in_size Must be 6
/// @param out Output registers in order eax ecx edx ebx esi edi
/// @param out_size Must be 6
/// @return An NTSTATUS
DEFINE_IOCTL_SIZED(ioctl_query_smm, 6, 6) {
    return dell_smm_call(in, out);
}

NTSTATUS:main() {
    if (get_arch() != ARCH_X64)
        return STATUS_NOT_SUPPORTED;

    // This SMM interface is Dell specific, and the requests forwarded by this
    // module are caller controlled. On anything else they would land in
    // unrelated firmware SMI handlers, so refuse to load unless the firmware
    // answers one of the two signature requests.
    if (!dell_smm_has_signature(SMM_GET_DELL_SIG1)
        && !dell_smm_has_signature(SMM_GET_DELL_SIG2))
        return STATUS_NOT_SUPPORTED;

    return STATUS_SUCCESS;
}
