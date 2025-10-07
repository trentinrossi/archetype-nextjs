'use client';

import React from 'react';
import { Button } from '@/components/ui/Button';

interface AdminMenuProps {
  onMenuSelect: (option: string) => void;
  onSignOut: () => void;
}

export function AdminMenu({ onMenuSelect, onSignOut }: AdminMenuProps) {
  // COADM01C business rule: Handle menu selections
  const handleMenuOption = (option: string) => {
    onMenuSelect(option);
  };

  return (
    <div className="min-h-screen bg-gray-100 p-4">
      <div className="bg-white rounded-lg shadow-md p-6 max-w-4xl mx-auto">
        {/* Screen Header - COADM01C layout */}
        <div className="mb-6 text-center border-b pb-4">
          <h1 className="text-2xl font-bold text-gray-800 mb-2">CARDDEMO</h1>
          <h2 className="text-lg text-gray-600 mb-4">Administration Menu</h2>
          <div className="text-sm text-gray-500 space-y-1">
            <div>Program: COADM01C | Transaction: CA01</div>
            <div>{new Date().toLocaleDateString('en-US', { 
              month: '2-digit', 
              day: '2-digit', 
              year: '2-digit' 
            })} {new Date().toLocaleTimeString('en-US', { 
              hour12: false 
            })}</div>
          </div>
        </div>

        {/* Menu Options */}
        <div className="space-y-4 mb-6">
          <h3 className="text-lg font-semibold text-gray-800 mb-4">User Management</h3>
          
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Button
              onClick={() => handleMenuOption('user-list')}
              variant="outline"
              className="h-16 text-left flex flex-col items-start justify-center"
            >
              <span className="font-medium">1. User List</span>
              <span className="text-sm text-gray-600">View and manage all users (COUSR00C)</span>
            </Button>

            <Button
              onClick={() => handleMenuOption('user-add')}
              variant="outline"
              className="h-16 text-left flex flex-col items-start justify-center"
            >
              <span className="font-medium">2. Add User</span>
              <span className="text-sm text-gray-600">Create new user account (COUSR01C)</span>
            </Button>

            <Button
              onClick={() => handleMenuOption('user-update')}
              variant="outline"
              className="h-16 text-left flex flex-col items-start justify-center"
            >
              <span className="font-medium">3. Update User</span>
              <span className="text-sm text-gray-600">Modify user information (COUSR02C)</span>
            </Button>

            <Button
              onClick={() => handleMenuOption('user-delete')}
              variant="outline"
              className="h-16 text-left flex flex-col items-start justify-center"
            >
              <span className="font-medium">4. Delete User</span>
              <span className="text-sm text-gray-600">Remove user account (COUSR03C)</span>
            </Button>
          </div>
        </div>

        {/* System Options */}
        <div className="border-t pt-4">
          <h3 className="text-lg font-semibold text-gray-800 mb-4">System Options</h3>
          
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Button
              onClick={() => handleMenuOption('reports')}
              variant="outline"
              className="h-16 text-left flex flex-col items-start justify-center"
              disabled
            >
              <span className="font-medium">5. Reports</span>
              <span className="text-sm text-gray-600">Generate system reports (Coming Soon)</span>
            </Button>

            <Button
              onClick={() => handleMenuOption('settings')}
              variant="outline"
              className="h-16 text-left flex flex-col items-start justify-center"
              disabled
            >
              <span className="font-medium">6. System Settings</span>
              <span className="text-sm text-gray-600">Configure system parameters (Coming Soon)</span>
            </Button>
          </div>
        </div>

        {/* Navigation Options */}
        <div className="mt-6 pt-4 border-t">
          <div className="flex justify-between">
            <Button
              onClick={onSignOut}
              variant="secondary"
            >
              Sign Out (PF3)
            </Button>

            <div className="text-sm text-gray-500 flex items-center">
              Administrator Menu - Select an option to continue
            </div>
          </div>
        </div>

        {/* Instructions */}
        <div className="mt-6 text-xs text-gray-500 text-center space-y-1">
          <p>Select a menu option to access the corresponding function</p>
          <p>Use Sign Out (PF3) to return to the main sign-on screen</p>
          <p>All user management functions follow COBOL program business rules</p>
        </div>
      </div>
    </div>
  );
}