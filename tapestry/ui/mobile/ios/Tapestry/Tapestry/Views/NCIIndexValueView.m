//
//  NCIIndexValueView.m
//  Tapestry
//
//  Created by Infoblox Inc on 11/12/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import "NCIIndexValueView.h"
#import "NCIHintView.h"

@interface NCIIndexValueView(){
    UILabel *indLabel;
    UILabel *indValue;
    UILabel *updateLabel;
    UIButton *helpBtn;
    UIView *contentView;
    float currentValue;
    
    float fontSize;
    NSString *indexName;

    NCIHintView *helpView;
}
@end

@implementation NCIIndexValueView

- (id)initWithFrame:(CGRect)frame indName:(NSString *)indName indSize:(float)size{
    self = [super initWithFrame:frame];
    if (self) {
        fontSize = size;
        indexName = indName;
        contentView = [[UIView alloc] initWithFrame:self.bounds];
        contentView.backgroundColor = [UIColor whiteColor];
        [self addSubview:contentView];
        indLabel = [[UILabel alloc] initWithFrame:CGRectZero];
        indLabel.text = indexName;
        indLabel.font = [UIFont boldSystemFontOfSize:fontSize];
        [contentView addSubview:indLabel];
        
        helpBtn = [[UIButton alloc] initWithFrame:CGRectZero];
        [helpBtn setImage:[UIImage imageNamed:@"question"] forState:UIControlStateNormal];
        [helpBtn addTarget:self action:@selector(showTooltip) forControlEvents:UIControlEventTouchUpInside];
        [contentView addSubview:helpBtn];
        
        indValue = [[UILabel alloc] initWithFrame:CGRectZero];
        indValue.font = [UIFont boldSystemFontOfSize:fontSize + 4];
        [contentView addSubview:indValue];
        updateLabel = [[UILabel alloc] initWithFrame:CGRectZero];
        updateLabel.font = [UIFont italicSystemFontOfSize:14];
        updateLabel.backgroundColor = [UIColor clearColor];
        [contentView addSubview:updateLabel];
        
        self.backgroundColor = [UIColor clearColor];
        contentView.backgroundColor = [UIColor clearColor];
        
        UITapGestureRecognizer *tap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(showTooltip)];
        tap.numberOfTapsRequired = 1;
        helpBtn.userInteractionEnabled = YES;
        [helpBtn addGestureRecognizer:tap];

    }
    return self;
}

-(void)showTooltip{
    [[NSNotificationCenter defaultCenter] postNotificationName:@"freeTap" object:self];
     helpView.hidden = NO;
}

- (void)resetData{
    currentValue = 0;
    updateLabel.text = @"";
    indValue.text = @"";
}

- (void)layoutSubviews{
    contentView.frame = self.bounds;
    indLabel.frame = CGRectMake(10, 5, self.frame.size.width, 30);
    CGFloat width =  [indLabel.text sizeWithFont:indLabel.font].width;
    helpBtn.frame = CGRectMake(width + 10, 0, 20, 20);
    indValue.frame = CGRectMake(width + 50, 0, 100, 30);
    updateLabel.frame = CGRectMake(10, 20, self.frame.size.width, 30);
}

- (void)setIndValue:(NSString *)value withDate:(NSString *)date{
    self.dateServerString = date;
    indValue.text = [self processIndexValue: value];
    updateLabel.text = [NSString stringWithFormat:NSLocalizedString(@"updated %@", nil), [NCIConstants processTime:date]];
}

- (void)setTooltipText:(NSString *)text{
    if (!helpView){
        helpView = [[NCIHintView alloc] initWithText:text andPoint:CGPointMake(30, 50)];
        [self addSubview:helpView];
    }
    //TODO
}

#pragma mark util methods

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
