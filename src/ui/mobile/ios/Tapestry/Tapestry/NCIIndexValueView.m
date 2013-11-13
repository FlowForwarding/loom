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
    UIButton *helpBtn;
    UIView *contentView;
    float currentValue;
    
    float fontSize;
    NSString *indexName;
}
@end

@implementation NCIIndexValueView

- (id)initWithFrame:(CGRect)frame indName:(NSString *)indName indSize:(float)size{
    self = [super initWithFrame:frame];
    if (self) {
        fontSize = size;
        indexName = indName;

    }
    return self;
}

- (void)layoutSubviews{
    contentView = [[UIView alloc] initWithFrame:self.bounds];
    contentView.backgroundColor = [UIColor whiteColor];
    [self addSubview:contentView];
    indLabel = [[UILabel alloc] initWithFrame:CGRectMake(10, 0, self.frame.size.width, 30)];
    indLabel.text = indexName;
    indLabel.font = [UIFont boldSystemFontOfSize:fontSize];
    [contentView addSubview:indLabel];
    CGFloat width =  [indLabel.text sizeWithFont:indLabel.font].width;
    
    helpBtn = [[UIButton alloc] initWithFrame:CGRectMake(width + 10, 0, 16, 16)];
    helpBtn.backgroundColor = [UIColor blackColor];
    helpBtn.titleLabel.font = [UIFont boldSystemFontOfSize:14];
    [helpBtn setTitle:@"?" forState:UIControlStateNormal];
    helpBtn.layer.cornerRadius = 8;
    [contentView addSubview:helpBtn];
    
    indValue = [[UILabel alloc] initWithFrame:CGRectMake(width + 50, 0, 100, 30)];
    indValue.font = [UIFont boldSystemFontOfSize:fontSize + 4];
    [contentView addSubview:indValue];
    updateLabel = [[UILabel alloc] initWithFrame:CGRectMake(10, 20, self.frame.size.width, 30)];
    updateLabel.font = [UIFont italicSystemFontOfSize:14];
    updateLabel.backgroundColor = [UIColor clearColor];
    [contentView addSubview:updateLabel];
}


- (void)setIndValue:(NSString *)value withDate:(NSString *)date{
    indValue.text = [self processIndexValue: value];
    updateLabel.text = [NSString stringWithFormat:NSLocalizedString(@"updated %@", nil), [self processTime:date]];
}

- (void)setTooltipText:(NSString *)text{
    //TODO
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
        indValue.textColor = [UIColor colorWithRed:45/255 green:105/255.0 blue:19/255.0 alpha:1]; //green color
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
