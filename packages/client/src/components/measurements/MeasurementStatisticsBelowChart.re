[@react.component]  
let make =  
    (
        ~cdf: ForetoldComponents.Types.Dist.t, 
        ~measurable: Types.measurable
    ) => {
    
    module Calculate = {
        let toPdf = (cdf: ForetoldComponents.Types.Dist.t) => {
            let ys =  cdf.ys;
            let ysPdf = Array.mapi((index, element) => {
            switch(index){
                | 0 => element;
                | someOtherIdex => element -. Array.get(ys,index-1);
            }

            }, ys); 
            
            ysPdf;
        } 
        let expectedValue = (cdf: ForetoldComponents.Types.Dist.t) => {
            let ys= toPdf(cdf);
            let result = Js.Array.reducei((accumulator, x, index) => (accumulator +. (x *. Array.get(ys,index)) ), 0.0, cdf.xs)
            result;
        } 

        let median = (cdf: ForetoldComponents.Types.Dist.t) => {
            let ys = cdf.ys;
            let pos = Js.Array.findIndex( y => y>=0.5, ys);
            pos==(-1) ? None : Some(cdf.xs[pos]);
        }

        let confidenceInterval = (cdf: ForetoldComponents.Types.Dist.t,p) => {
            let ys = cdf.ys;
            let low_p = ((100.0-.p)/.100.0)/.2.0;
            let high_p = 1.0-.low_p;
            
            let low_value_index = Js.Array.findIndex( y => y>=low_p, ys);
            let high_value_index = Js.Array.findIndex( y => y>=high_p, ys);

            let low_value = Array.get(cdf.xs,low_value_index);
            let high_value = Array.get(cdf.xs,high_value_index);

            (low_value, high_value);
        }      

        let highestLikelihood = (cdf: ForetoldComponents.Types.Dist.t) => {
            let ysPdf = toPdf(cdf) 
            let indexOfMaxValue = Js.Array.reducei((iMax, x, i) => (x > Array.get(ysPdf,iMax))? i : iMax, 0, ysPdf);
            
            Array.get(cdf.xs,indexOfMaxValue);
        }
    
    };

    module Display = {
        let tableCss = ReactDOMRe.Style.make(~height="10px", ~borderSpacing="10px 0", ~borderCollapse="separate", ());

        let oneDigit = (value:float) => Js.Float.toFixedWithPrecision(value, ~digits=1) 
        let genericDisplay = (floatValue:float) => {
            floatValue
            -> oneDigit
            -> React.string;
        }
        
        let expectedValue = (cdf: ForetoldComponents.Types.Dist.t) => {
            Calculate.expectedValue(cdf)
            -> genericDisplay;
        }

        let median = (cdf: ForetoldComponents.Types.Dist.t) => {
            let resultString = switch(Calculate.median(cdf)){
            | None => "Median not found"
            | Some(value) => oneDigit(value)
            };
            resultString -> React.string;
        }

        let confidenceInterval = (cdf: ForetoldComponents.Types.Dist.t,p) => {
            let (low_value, high_value) = Calculate.confidenceInterval(cdf,p);
            let resultString = oneDigit(low_value) ++ " to " ++ oneDigit(high_value);
            resultString -> React.string;
        }

        let highestLikelihood = (cdf: ForetoldComponents.Types.Dist.t) => {
            Calculate.highestLikelihood(cdf)
            -> genericDisplay;
        }

    };
    
    <table style={Display.tableCss}>
        <thead>
            <tr>
                <th>{React.string("")}</th>
                <th>{React.string("Your distribution")}</th> 
                //<th>{React.string("Aggregate")}</th>
            </tr>
        </thead>
        <tbody>
            <tr>
                <td>{React.string("Expected value")}</td>
                <td>{Display.expectedValue(cdf)}</td> 
                //<td>{React.string("Aggregation Value")}</td>
            </tr>
            <tr>
                <td>{React.string("Median")}</td>
                <td>{Display.median(cdf)}</td> 
                //<td>{React.string("Aggregation Value")}</td>
            </tr>
            <tr>
                <td>{React.string("90% confidence interval")}</td>
                <td>{Display.confidenceInterval(cdf, 90.0)}</td> 
                //<td>{React.string("Aggregation Value")}</td>
            </tr>
            <tr>
                <td>{React.string("50% confidence interval")}</td>
                <td>{Display.confidenceInterval(cdf, 50.0)}</td> 
                //<td>{React.string("Aggregation Value")}</td>
            </tr>
            <tr>
                <td>{React.string("Highest likelihood")}</td>
                <td>{Display.highestLikelihood(cdf)}</td> 
                //<td>{React.string("Aggregation Value")}</td>
            </tr>        
            
        </tbody>
    </table>;

    /* <p>{React.string("The lone and level sands stretch far away")}</p>; */
}
