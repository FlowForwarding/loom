//
//  NCITapestryController.m
//  Tapestry
//
//  Created by Ira on 6/18/14.
//  Copyright (c) 2014 Ira. All rights reserved.
//

#import "NCITapestryController.h"
#import "NCIEditServerView.h"

@interface NCITapestryController ()

@end

@implementation NCITapestryController

- (void)viewDidLoad
{
    [super viewDidLoad];
    float selfWidth = self.view.frame.size.width;
    float selfHeight = self.view.frame.size.height;
    float searchFieldHeight = 80;
    
    UIWebView *webContent = [[UIWebView alloc] initWithFrame:CGRectMake(0, searchFieldHeight, selfHeight, selfWidth - searchFieldHeight)];
    [self.view addSubview:webContent];
    NSURL *url = [NSURL fileURLWithPath:[[NSBundle mainBundle] pathForResource:@"nci" ofType:@"html" inDirectory:@"www"] isDirectory:NO];
    NSURLRequest *request = [NSURLRequest requestWithURL:url];
    
    NCIEditServerView *serverEditor = [[NCIEditServerView alloc] initWithFrame:
                                       CGRectMake(0, 0, selfHeight, searchFieldHeight) andView:webContent];
    [self.view addSubview:serverEditor];
    [webContent loadRequest:request];

    
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

/*
#pragma mark - Navigation

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    // Get the new view controller using [segue destinationViewController].
    // Pass the selected object to the new view controller.
}
*/

@end
