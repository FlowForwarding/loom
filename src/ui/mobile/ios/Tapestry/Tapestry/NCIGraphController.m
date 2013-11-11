//
//  NCIGraphController.m
//  Tapestry
//
//  Created by Ira on 11/11/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIGraphController.h"

@interface NCIGraphController ()

@end

@implementation NCIGraphController

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    self.title = NSLocalizedString(@"Tapestry: A Network Complexity Analyzer", nil);
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

@end
