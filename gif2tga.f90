program gif2tga
    implicit none

    character*256 filename
    integer, parameter :: unit = 1
    integer i

    integer width
    integer height
    integer*1 palette(3, 0:255)
    integer*1, allocatable :: pixels(:)

    integer*1 block_bytes
    integer bit_buffer
    integer bit_buffer_bits
    integer code_bits
    integer code

    call get_command_argument(1, filename)
    open (unit, file=filename, access="STREAM", action="READ", form="UNFORMATTED")
    call read_gif()
    close (unit)

    i = index(filename, ".", .true.) ! replace the filename extension
    if (i <= 0) i = len_trim(filename) + 1 ! or append
    filename(i:) = ".tga"
    open (unit, file=filename, access="STREAM", action="WRITE", form="UNFORMATTED")
    call write_tga()
    close (unit)

contains

    subroutine read_palette(flags)
        integer*1, value :: flags
        integer colors

        if (btest(flags, 7)) then
            colors = ishft(2, iand(flags, 7))
            read (unit) palette(:, 0 : colors - 1)
        end if
    end

    function read_block_byte()
        integer*1 read_block_byte

        if (block_bytes == 0) then
            read (unit) block_bytes
            if (block_bytes == 0) stop "Unexpected block terminator"
        end if
        block_bytes = int(block_bytes - 1, 1)
        read (unit) read_block_byte
    end

    subroutine read_code()
        integer b

        do while (bit_buffer_bits < code_bits)
            b = read_block_byte()
            ! mvbits equivalent to: bit_buffer = bit_buffer + ishft(iand(b, 255), bit_buffer_bits)
            call mvbits(b, 0, 8, bit_buffer, bit_buffer_bits)
            bit_buffer_bits = bit_buffer_bits + 8
        end do
        code = iand(bit_buffer, ishft(1, code_bits) - 1)
        bit_buffer = ishft(bit_buffer, -code_bits)
        bit_buffer_bits = bit_buffer_bits - code_bits
    end

    subroutine read_gif()
        character*6 signature
        integer*1 header(7)
        integer*1 b
        integer*1 block_length
        integer i
        integer*2 image_descriptor(4)
        integer*1 literal_bits
        integer pixels_length
        integer literal_codes
        integer codes
        integer, parameter :: max_codes = 4096
        integer offsets(max_codes + 1)
        integer pixels_offset
        integer source_offset
        integer source_end_offset
        integer dest_end_offset

        ! Read file header
        read (unit) signature, header
        if (signature /= "GIF87a" .and. signature /= "GIF89a") STOP "Not a GIF"
        call read_palette(header(5))

        ! Skip extension blocks if any
        do
            read (unit) b
            if (b == z'2c') exit ! Image Descriptor
            if (b /= z'21') stop "Invalid block" ! Extension
            read (unit) b
            do
                read (unit) block_length
                if (block_length == 0) exit
                do i=1, block_length
                    read (unit) b
                end do
            end do
        end do

        ! Read image descriptor
        read (unit) image_descriptor
        width = image_descriptor(3)
        height = image_descriptor(4)
        if (width == 0 .or. height == 0) stop "Zero size"
        read (unit) b
        if (btest(b, 6)) stop "Interlace not supported"
        call read_palette(b)
        read (unit) literal_bits
        if (literal_bits == 0 .or. literal_bits > 8) stop "Invalid minimum code size"

        block_bytes = 0
        bit_buffer = 0
        bit_buffer_bits = 0
        pixels_length = width * height
        allocate(pixels(0 : pixels_length - 1))
        literal_codes = ishft(1, literal_bits)
        codes = literal_codes + 2
        code_bits = literal_bits + 1
        pixels_offset = 0
        do while (pixels_offset < pixels_length)
            call read_code()
            if (code == literal_codes) then
                ! reset
                codes = literal_codes + 2
                code_bits = literal_bits + 1
                cycle
            end if
            if (code == literal_codes + 1) stop "Unexpected EOI code"
            if (code >= codes) stop "Code out of range"
            if (code <= max_codes) then
                if (code < max_codes .and. iand(codes, codes - 1) == 0) code_bits = code_bits + 1
                offsets(codes) = pixels_offset
                codes = codes + 1
            end if
            if (code < literal_codes) then
                pixels(pixels_offset) = int(code, 1)
                pixels_offset = pixels_offset + 1
            else
                source_offset = offsets(code)
                source_end_offset = offsets(code + 1)
                dest_end_offset = pixels_offset + (source_end_offset - source_offset)
                pixels(pixels_offset:dest_end_offset - 1) = pixels(source_offset:source_end_offset - 1)
                pixels(dest_end_offset) = pixels(source_end_offset)
                pixels_offset = dest_end_offset + 1
            end if
        end do
    end

    subroutine write_tga()
        integer*1 header(18)
        header = int( (/ 0, 1, 1, 0, 0, 0, 1, 24, 0, 0, 0, 0, &
            width, ishft(width, -8), height, ishft(height, -8), 8, 32 /), 1)
        write (unit) header, palette(3:1:-1, :), pixels ! palette in BGR order
    end
end
