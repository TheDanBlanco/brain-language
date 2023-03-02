use proc_macro::TokenStream;

pub fn parse(stream: TokenStream) -> TokenStream {
    let stream_iter = &mut stream.clone().into_iter();

    while let Some(token) = stream_iter.next() {
        println!("{}", token);
    }

    stream
}