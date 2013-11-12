//
//  NCIIndexValueView.m
//  Tapestry
//
//  Created by Ira on 11/12/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIIndexValueView.h"

@interface NCIIndexValueView(){
    UILabel *indLabel;
    UILabel *indValue;
    UILabel *updateLabel;
    float currentValue;
}
@end

@implementation NCIIndexValueView

- (id)initWithFrame:(CGRect)frame indName:(NSString *)indName indSize:(float)size{
    self = [super initWithFrame:frame];
    if (self) {
        indLabel = [[UILabel alloc] initWithFrame:CGRectMake(10, 0, 100, 30)];
        indLabel.text = indName;
        [self addSubview:indLabel];
        indValue = [[UILabel alloc] initWithFrame:CGRectMake(100, 0, 100, 30)];
        [self addSubview:indValue];
        updateLabel = [[UILabel alloc] initWithFrame:CGRectMake(10, 20, self.frame.size.width, 30)];
        updateLabel.font = [UIFont italicSystemFontOfSize:14];
        [self addSubview:updateLabel];
    }
    return self;
}

- (void)setIndValue:(NSString *)value withDate:(NSString *)date{
    indValue.text = [self processIndexValue: value];
    updateLabel.text = [NSString stringWithFormat:NSLocalizedString(@"updated %@", nil), [self processTime:date]];
}

#pragma mark util methods

- (NSString *)processTime:(NSString *) time{
    return [[time stringByReplacingOccurrencesOfString:@"T" withString:@" "]
            stringByReplacingOccurrencesOfString:@"Z" withString:@""];
}

- (NSString *)processIndexValue:(NSString *)indexValue
{
    float val = [indexValue floatValue];
    if (!currentValue || currentValue == val){
        indValue.textColor = [UIColor blackColor];
    } else if (val > currentValue) {
        indValue.textColor = [UIColor greenColor];
    } else {
        indValue.textColor = [UIColor redColor];
    };
    currentValue = val;
    bool fixed = !(val - (int)val == 0);
    NSString *size = @"";
    if (val >  1.0e+9) {
        val = val / 1.0e+9;
        size = @"B";
        fixed = YES;
    } else if (val > 1.0e+6){
        val = val / 1.0e+6;
        size = @"M";
        fixed = YES;
    } else if (val > 1.0e+3){
        val = val / 1.0e+3;
        size = @"K";
        fixed = YES;
    }
    if (fixed){
        return [NSString stringWithFormat:@"%.1f %@", val, size];
    } else {
        return [NSString stringWithFormat:@"%.0f %@", val, size];
    }
}

@end
