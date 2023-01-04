pub mod day13;
pub mod day19;
pub mod day23;
pub mod day24;
pub mod day7;

#[cfg(feature = "systemd")]
pub mod systemd;

// for measurement program
pub fn day_part_iterator() -> impl Iterator<Item = (u8, u8)> {
    (1..=24)
        .flat_map(|day| (1..=2).map(move |part| (day, part)))
        .chain((25..26).map(|day| (day, 1)))
}

// assuming buf is large enough
// otherwise it will panic
pub fn u32_to_bytes(x: u32, buf: &mut [u8]) -> usize {
    if x == 0 {
        buf[0] = b'0';
        1
    } else {
        let mut i = 0;
        let mut x = x;
        while x > 0 {
            buf[i] = (x % 10) as u8 + b'0';
            x /= 10;
            i += 1;
        }
        // reverse
        let mut j = 0;
        while j < i / 2 {
            buf.swap(j, i - j - 1);
            j += 1;
        }
        i
    }
}

#[test]
fn test_u32_to_bytes() {
    let mut buf = [0u8; 10];
    assert_eq!(u32_to_bytes(0, &mut buf), 1);
    assert_eq!(buf[0], b'0');
    assert_eq!(u32_to_bytes(1, &mut buf), 1);
    assert_eq!(buf[0], b'1');
    assert_eq!(u32_to_bytes(123, &mut buf), 3);
    assert_eq!(buf[0], b'1');
    assert_eq!(buf[1], b'2');
    assert_eq!(buf[2], b'3');
    assert_eq!(u32_to_bytes(1234567890, &mut buf), 10);
    assert_eq!(buf[0], b'1');
    assert_eq!(buf[1], b'2');
    assert_eq!(buf[2], b'3');
    assert_eq!(buf[3], b'4');
    assert_eq!(buf[4], b'5');
    assert_eq!(buf[5], b'6');
    assert_eq!(buf[6], b'7');
    assert_eq!(buf[7], b'8');
    assert_eq!(buf[8], b'9');
    assert_eq!(buf[9], b'0');
}
