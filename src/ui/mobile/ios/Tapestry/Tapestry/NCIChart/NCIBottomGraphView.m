//
//  NCIBottomGraphView.m
//  Tapestry
//
//  Created by Infoblox Inc on 11/19/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import "NCIBottomGraphView.h"
#import "NCIHandspikeView.h"
#import "NCIMianGraphView.h"
#import "NCIWebSocketConnector.h"

@interface NCIBottomGraphView(){
    NCIHandspikeView *handspikeLeft;
    NCIHandspikeView *handspikeRight;
    UIView *rightAmputation;
    UIView *leftAmputation;
    
    float handspikeWidth;
    float gridWidth;
    float gridStep; //number of seconds for pixel
    float handspikeIndent;
    float gridHeigth;
    
    //values on start dragging ranges
    float fixedLeftVal;
    float fixedRightVal;
    
    UIView *fake;
    float minRangesDistance;
}

@property(nonatomic)float xHandspikeLeft;
@property(nonatomic)float xHandspikeRight;

@end

@implementation NCIBottomGraphView

- (id)initWithChart: (NCIChartView *)chartHolder{
    self = [super initWithChart:chartHolder];
    if (self){
        self.chart = chartHolder;
        self.hasGrid = NO;
        self.hasYLabels = NO;
        self.topChartIndent = 0;
        minRangesDistance = 3;
    
        //fake view to enable touches in space between ranges
        fake = [[UIView alloc] initWithFrame:CGRectZero];
        [self addSubview:fake];
        
        leftAmputation = [[UIView alloc] initWithFrame:CGRectZero];
        leftAmputation.backgroundColor = [UIColor colorWithWhite:0.3 alpha:0.1];
        [self addSubview:leftAmputation];
        
        handspikeLeft = [[NCIHandspikeView alloc] initWithFrame:CGRectZero];
        [self addSubview:handspikeLeft];
        
        rightAmputation = [[UIView alloc] initWithFrame:CGRectZero];
        rightAmputation.backgroundColor = [UIColor colorWithWhite:0.3 alpha:0.1];
        [self addSubview:rightAmputation];
        
        handspikeRight = [[NCIHandspikeView alloc] initWithFrame:CGRectZero];
        [self addSubview:handspikeRight];
        
        handspikeWidth = 4;
        self.multipleTouchEnabled = YES;
    }
    return self;
}

- (void)layoutSubviews{
    [super layoutSubviews];
    fake.frame = self.bounds;
    gridWidth = self.frame.size.width - 2*self.leftRightIndent;
    gridStep = gridWidth/([self.chart getMaxArgument] - [self.chart getMinArgument]);
    handspikeIndent = self.leftRightIndent - handspikeWidth/2;
    gridHeigth = self.frame.size.height - self.bottomChartIndent - self.topChartIndent;
    
    self.scaleIndex = 1;
    self.gridScroll.frame = CGRectMake(self.leftRightIndent,self.topChartIndent, self.frame.size.width - 2*self.leftRightIndent,
                                       self.frame.size.height - self.topChartIndent);
    self.gridScroll.contentSize =
    CGSizeMake((self.frame.size.width - 2*self.leftRightIndent)*self.scaleIndex, self.frame.size.height - self.topChartIndent - self.bottomChartIndent);
    self.gridArea.frame = CGRectMake(0, 0, (self.frame.size.width - 2*self.leftRightIndent)*self.scaleIndex, self.frame.size.height - self.topChartIndent - self.bottomChartIndent);
    [self.gridScroll setContentOffset:CGPointMake((self.scaleIndex -1)*(self.frame.size.width - 2*self.leftRightIndent - self.leftShift*self.scaleIndex), 0)];
    [self redrawRanges];

}

- (void)redrawRanges{
    
    if (self.chart.chartData.count < 1)
        return;
    
    gridWidth = self.frame.size.width - 2*self.leftRightIndent;
    
    if (!_xHandspikeLeft){
        _xHandspikeLeft = handspikeIndent;
        fixedLeftVal = _xHandspikeLeft;
    };
    
    if (!_xHandspikeRight){
        _xHandspikeRight = handspikeIndent + gridWidth ;
        fixedRightVal = _xHandspikeRight;
    };
    
    if (self.chart.minRangeDate ){
        _xHandspikeLeft = handspikeIndent + ([self.chart.minRangeDate timeIntervalSince1970] - [self.chart getMinArgument])*gridStep;
        _xHandspikeRight = handspikeIndent + ([self.chart.maxRangeDate timeIntervalSince1970] - [self.chart getMinArgument])*gridStep;
    }
    
    if (_xHandspikeLeft < handspikeIndent){
        _xHandspikeLeft = handspikeIndent;
    }
    
    if (_xHandspikeRight > (gridWidth + handspikeIndent)){
        _xHandspikeRight = gridWidth + handspikeIndent;
    }
    
    if (_xHandspikeRight < (handspikeIndent +  minRangesDistance)){
        _xHandspikeRight = (handspikeIndent +  minRangesDistance);
    }
    
    if ((_xHandspikeRight - _xHandspikeLeft) < minRangesDistance){
        _xHandspikeRight = _xHandspikeLeft + minRangesDistance;
    }
    
    handspikeLeft.frame = CGRectMake(_xHandspikeLeft, self.topChartIndent, handspikeWidth, gridHeigth);
    leftAmputation.frame = CGRectMake(self.leftRightIndent, self.topChartIndent, _xHandspikeLeft - handspikeIndent, gridHeigth);
    
    handspikeRight.frame = CGRectMake(_xHandspikeRight, self.topChartIndent, handspikeWidth, gridHeigth);
    rightAmputation.frame = CGRectMake(_xHandspikeRight + handspikeWidth/2, self.topChartIndent,
                                       gridWidth + handspikeIndent - _xHandspikeRight, gridHeigth);
    [self.chart.mainGraph setNeedsLayout];
    [self.chart.mainGraph setNeedsDisplay];
    
}

static float startLeft = -1;
static float startLeftRange = -1;
static float startRight = -1;
static float startRightRange = -1;

-(void) touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    __block UITouch *touch1;
    __weak NCIBottomGraphView *weakSelf = self;
    
    [[event allTouches] enumerateObjectsUsingBlock:^(UITouch  *touch, BOOL *stop) {
        CGPoint location = [touch locationInView:weakSelf];
        if ([event allTouches].count == 2 ){
            if (!touch1){
                touch1 = touch;
            } else {
                [weakSelf startMoveWithPoint:[touch1 locationInView:weakSelf] andPoint:location];
            }
        } else {
            if (location.x <= (_xHandspikeLeft + handspikeWidth)){
                startLeft = location.x - handspikeLeft.center.x;
            } else if (location.x >= (_xHandspikeRight - handspikeWidth)){
                startRight = location.x - handspikeRight.center.x;
            }
        }
    }];
    
}
- (void) touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event{
    if (self.chart.chartData.count < 2)
        return;
    __block UITouch *touch1;
    
    __block float newLeft = -1;
    __block float newRight = -1;
    
    __weak NCIBottomGraphView* weakSelf = self;
    //here we set up new min and max ranges values for chart
    [[event allTouches] enumerateObjectsUsingBlock:^(UITouch  *touch, BOOL *stop) {
        CGPoint location = [touch locationInView:weakSelf];
        if ([event allTouches].count == 2 ){
            if (!touch1){
                touch1 = touch;
            } else {
                NSArray *newXPos = [weakSelf detectNewXPosFrom:[touch1 locationInView:weakSelf] and:location];
                newLeft = [(NSNumber *)newXPos[0] doubleValue];
                newRight = [(NSNumber *)newXPos[1] doubleValue];
            }
            
        } else {
            if (location.x <= (_xHandspikeLeft + handspikeWidth)){
                newLeft = location.x - startLeft;
            } else if (location.x >= (_xHandspikeRight - handspikeWidth)){
                newRight = location.x - startRight;
            }
        };
        
    }];
    
    [self moveRangesFollowingNewLeft:newLeft newRight:newRight];
    [[NCIWebSocketConnector interlocutor].periodSwitcherPanel resetButtons];
}

- (void)startMoveWithPoint:(CGPoint) point1 andPoint:(CGPoint) point2{
    startLeftRange = handspikeLeft.center.x;
    startRightRange = handspikeRight.center.x;
    if(point1.x < point2.x){
        startLeft = point1.x - handspikeLeft.center.x;
        startRight = point2.x - handspikeRight.center.x;
    } else {
        startLeft = point2.x - handspikeLeft.center.x;
        startRight = point1.x - handspikeRight.center.x;
    }
    [[NCIWebSocketConnector interlocutor].periodSwitcherPanel resetButtons];
}

- (NSArray *)detectNewXPosFrom:(CGPoint)location1 and:(CGPoint) location2{
    if (location1.x < location2.x){
        return @[[NSNumber numberWithDouble:location1.x - startLeft], [NSNumber numberWithDouble:location2.x - startRight]];
    } else {
        return @[[NSNumber numberWithDouble:location2.x - startLeft], [NSNumber numberWithDouble:location1.x - startRight]];
    }
}

- (void)moveRangesFollowingNewLeft:(double)newLeft newRight:(double)newRight {
    if ((newLeft != -1 && newRight != -1) && (newRight - newLeft) < minRangesDistance)
        return;
    
    if ( (newLeft != -1 ) && ((newLeft - self.leftRightIndent) > 0)){
        if ((_xHandspikeRight - newLeft) < minRangesDistance)
            return;
        self.chart.minRangeDate = [self dateFromXPos:newLeft];
    };
    
    if ((newRight != -1) && ((newRight + self.leftRightIndent) < self.frame.size.width)){
        if ((newRight - _xHandspikeLeft) < minRangesDistance)
            return;
        self.chart.maxRangeDate = [self dateFromXPos:newRight];
    };
    
    [self redrawRanges];
}

//these 2 reverse methods are for Main Graph pinch gesures
- (void)moveReverseRangesWithPoint:(CGPoint) point1 andPoint:(CGPoint) point2{
    NSArray *newXPos = [self detectNewReverseXPosFrom:point1 and:point2];
    [self moveRangesFollowingNewLeft:[(NSNumber *)newXPos[0] doubleValue] newRight:[(NSNumber *)newXPos[1] doubleValue]];
}

- (NSArray *)detectNewReverseXPosFrom:(CGPoint)location1 and:(CGPoint) location2{
    if (location1.x < location2.x){
        return @[[NSNumber numberWithDouble:startLeftRange - ( (location1.x - startLeft) - startLeftRange )],
                 [NSNumber numberWithDouble:startRightRange + (startRightRange -(location2.x - startRight))]];
    } else {
        return @[[NSNumber numberWithDouble:startLeftRange - ( (location2.x - startLeft) - startLeftRange)],
                 [NSNumber numberWithDouble:startRightRange + (startRightRange - (location1.x - startRight))]];
    }
}


- (NSDate *)dateFromXPos:(float)xPos{
    return [NSDate dateWithTimeIntervalSince1970:[self.chart getMinArgument] + (xPos - self.leftRightIndent)/gridStep];
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event{
}


@end
