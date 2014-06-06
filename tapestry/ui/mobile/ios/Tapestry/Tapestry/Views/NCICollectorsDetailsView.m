//
//  NCICollectorsDetailsView.m
//  Tapestry
//
//  Created by Ira on 5/16/14.
//  Copyright (c) 2014 FlowForwarding.Org. All rights reserved.
//

#import "NCICollectorsDetailsView.h"

@interface NCICollectorsDetailsView()<UITableViewDelegate, UITableViewDataSource>{
    UITableView *collectorsTable;
    UILabel *headerLabel;
}
@property(nonatomic, strong) NSArray *collectors;
@end

static NSString *CellIdentifier = @"CollectorCell";
float collectorsTableHeaderHeight = 60;
float collectorsCellHeight = 60;
float headerHeight = 60;

@implementation NCICollectorsDetailsView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        collectorsTable = [[UITableView alloc] initWithFrame:
                           CGRectMake(0, headerHeight, self.content.frame.size.width, self.content.frame.size.height)];
        collectorsTable.delegate = self;
        collectorsTable.dataSource = self;
        collectorsTable.separatorStyle = UITableViewCellSeparatorStyleNone;
        [self.content addSubview:collectorsTable];
        UIColor *mainColor = [UIColor colorWithRed:95/255.0 green:116/255.0 blue:126/255.0 alpha:1];
        
        headerLabel = [[UILabel alloc] initWithFrame:CGRectMake(0, 0, self.content.frame.size.width, headerHeight)];
        headerLabel.textAlignment = NSTextAlignmentCenter;
        headerLabel.textColor = [UIColor whiteColor];
        CAGradientLayer *headerLayer = [CAGradientLayer layer];
        headerLayer.frame = headerLabel.bounds;
        headerLayer.colors = @[(id)[mainColor colorWithAlphaComponent:0.6].CGColor, (id)[mainColor colorWithAlphaComponent:0.9].CGColor];
        
        UIView *labelBackground = [[UIView alloc] initWithFrame:headerLabel.frame];
        [labelBackground.layer addSublayer:headerLayer];
        [self.content addSubview:labelBackground];
        [self.content addSubview:headerLabel];
    }
    return self;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section{
    return _collectors.count;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath{
    
    UITableViewCell *cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleSubtitle reuseIdentifier:CellIdentifier];
    //TODO optimize, refactor
//    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
//    if (cell == nil) {
//        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleSubtitle reuseIdentifier:CellIdentifier];
//    }
    NSDictionary *collector = [_collectors objectAtIndex:indexPath.row];
    NSArray *columnIds = @[@"name", @"collector_type", @"ip", @"datapath_id", @"qps"];
    float columnWidth = self.frame.size.width/5;
    for (int i= 0; i < columnIds.count; i++){
        UILabel *columnLabel = [[UILabel alloc] initWithFrame:
                                CGRectMake(columnWidth*i, 0, columnWidth, collectorsTableHeaderHeight)];
        columnLabel.text = collector[columnIds[i]];
        columnLabel.textAlignment = NSTextAlignmentCenter;
        [cell addSubview:columnLabel];
    }
    return cell;
}

- (UIView *)tableView:(UITableView *)tableView viewForHeaderInSection:(NSInteger)section{
    UIView *header = [[UIView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width, collectorsTableHeaderHeight)];
    header.backgroundColor =  [UIColor colorWithRed:95/255.0 green:116/255.0 blue:126/255.0 alpha:1];
    NSArray *columnNames = @[@"Collector", @"Type", @"IP Address", @"Datapath ID", @"QPS"];
    float columnWidth = self.frame.size.width/5;
    for (int i= 0; i < columnNames.count; i++){
        UILabel *columnLabel = [[UILabel alloc] initWithFrame:
                                CGRectMake(columnWidth*i, 0, columnWidth, collectorsTableHeaderHeight)];
        columnLabel.text = columnNames[i];
        columnLabel.textAlignment = NSTextAlignmentCenter;
        columnLabel.textColor = [UIColor whiteColor];
        [header addSubview:columnLabel];
    }
    return  header;
}

- (void)loadData:(NSDictionary *)data{
    _collectors = data[@"Collectors"];
    [collectorsTable reloadData];
    headerLabel.text = [NSString stringWithFormat:@"%d collecotr(s) at %@", _collectors.count, [NCIConstants processTime:data[@"Time"]]];
}

- (CGFloat)tableView:(UITableView *)tableView heightForHeaderInSection:(NSInteger)section{
    return collectorsTableHeaderHeight;
}

- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath{
    return collectorsCellHeight;
}

- (void)hideActions{
    self.collectors = @[];
    headerLabel.text = @"";
    [collectorsTable reloadData];
}

@end
