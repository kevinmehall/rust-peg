use arithmetic::{expression};
mod arithmetic;

fn main() {
	assert_eq!(expression("1+1"), Ok(2));
	assert_eq!(expression("5*5"), Ok(25));
	assert_eq!(expression("222+3333"), Ok(3555));
	assert_eq!(expression("2+3*4"), Ok(14));
	assert_eq!(expression("(2+2)*3"), Ok(12));
	assert!(expression("(22+)+1").is_err());
	assert!(expression("1++1").is_err());
	assert!(expression("3)+1").is_err());
}