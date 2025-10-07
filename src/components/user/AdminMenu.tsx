'use client';

import React, { useState } from 'react';
import { Button } from '@/components/ui/Button';

interface AdminMenuProps {
  onMenuSelect: (option: 'list' | 'add' | 'update' | 'delete' | 'exit') => void;
  userType: 'ADMIN' | 'GENERAL';
}

export function AdminMenu({ onMenuSelect, userType }: AdminMenuProps) {
  const [message, setMessage] = useState('');
  const [messageType, setMessageType] = useState<'success' | 'error' | 'info'>('info');

  // Handle menu selection
  const handleMenuSelect = (option: 'list' | 'add' | 'update' | 'delete' | 'exit') => {
    if (userType !== 'ADMIN' && option !== 'exit') {
      setMessage('Access denied. Administrator privileges required.');
      setMessageType('error');
      return;
    }
    
    setMessage('');
    onMenuSelect(option);
  };

  // Handle key press events (simulating COBOL function keys)
  const handleKeyPress = (e: React.KeyboardEvent) => {
    if (e.key === '1') {
      e.preventDefault();
      handleMenuSelect('list');
    } else if (e.key === '2') {
      e.preventDefault();
      handleMenuSelect('add');
    } else if (e.key === '3') {
      e.preventDefault();
      handleMenuSelect('update');
    } else if (e.key === '4') {
      e.preventDefault();
      handleMenuSelect('delete');
    } else if (e.key === 'F3' || (e.altKey && e.key === '3')) {
      e.preventDefault();
      handleMenuSelect('exit');
    }
  };

  return (
    <div className="min-h-screen bg-gray-900 text-green-400 font-mono" onKeyDown={handleKeyPress} tabIndex={0}>
      {/* Header */}
      <div className="bg-gray-800 p-4 border-b border-green-400">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-xl font-bold">CARDDEMO</h1>
            <h2 className="text-lg">Administration Menu</h2>
          </div>
          <div className="text-right">
            <div>TRAN: CADM</div>
            <div>PGM: COADM01C</div>
            <div>{new Date().toLocaleDateString('en-US', { 
              month: '2-digit', 
              day: '2-digit', 
              year: '2-digit' 
            })}</div>
            <div>{new Date().toLocaleTimeString('en-US', { 
              hour12: false,
              hour: '2-digit',
              minute: '2-digit',
              second: '2-digit'
            })}</div>
          </div>
        </div>
      </div>

      {/* Main menu area */}
      <div className="p-8">
        <div className="max-w-2xl mx-auto">
          {/* User info */}
          <div className="mb-8 p-4 border border-green-400 bg-gray-800">
            <h3 className="text-lg font-bold mb-2">User Information</h3>
            <div>User Type: {userType === 'ADMIN' ? 'Administrator' : 'General User'}</div>
            <div>Access Level: {userType === 'ADMIN' ? 'Full Access' : 'Limited Access'}</div>
          </div>

          {/* Menu options */}
          <div className="space-y-4">
            <h3 className="text-lg font-bold mb-4">User Management Options:</h3>
            
            <div className="grid grid-cols-1 gap-4">
              <Button
                onClick={() => handleMenuSelect('list')}
                disabled={userType !== 'ADMIN'}
                className={`w-full text-left p-4 h-auto ${
                  userType === 'ADMIN' 
                    ? 'bg-green-700 hover:bg-green-600 text-white' 
                    : 'bg-gray-700 text-gray-400 cursor-not-allowed'
                }`}
              >
                <div className="flex items-center justify-between">
                  <div>
                    <div className="font-bold">1. User List Management</div>
                    <div className="text-sm opacity-80">View, search, and manage user accounts</div>
                  </div>
                  <div className="text-2xl">→</div>
                </div>
              </Button>

              <Button
                onClick={() => handleMenuSelect('add')}
                disabled={userType !== 'ADMIN'}
                className={`w-full text-left p-4 h-auto ${
                  userType === 'ADMIN' 
                    ? 'bg-green-700 hover:bg-green-600 text-white' 
                    : 'bg-gray-700 text-gray-400 cursor-not-allowed'
                }`}
              >
                <div className="flex items-center justify-between">
                  <div>
                    <div className="font-bold">2. Add New User</div>
                    <div className="text-sm opacity-80">Create a new user account</div>
                  </div>
                  <div className="text-2xl">→</div>
                </div>
              </Button>

              <Button
                onClick={() => handleMenuSelect('update')}
                disabled={userType !== 'ADMIN'}
                className={`w-full text-left p-4 h-auto ${
                  userType === 'ADMIN' 
                    ? 'bg-green-700 hover:bg-green-600 text-white' 
                    : 'bg-gray-700 text-gray-400 cursor-not-allowed'
                }`}
              >
                <div className="flex items-center justify-between">
                  <div>
                    <div className="font-bold">3. Update User</div>
                    <div className="text-sm opacity-80">Modify existing user account details</div>
                  </div>
                  <div className="text-2xl">→</div>
                </div>
              </Button>

              <Button
                onClick={() => handleMenuSelect('delete')}
                disabled={userType !== 'ADMIN'}
                className={`w-full text-left p-4 h-auto ${
                  userType === 'ADMIN' 
                    ? 'bg-red-700 hover:bg-red-600 text-white' 
                    : 'bg-gray-700 text-gray-400 cursor-not-allowed'
                }`}
              >
                <div className="flex items-center justify-between">
                  <div>
                    <div className="font-bold">4. Delete User</div>
                    <div className="text-sm opacity-80">Remove a user account from the system</div>
                  </div>
                  <div className="text-2xl">→</div>
                </div>
              </Button>
            </div>

            {/* Exit option */}
            <div className="mt-8 pt-4 border-t border-green-400">
              <Button
                onClick={() => handleMenuSelect('exit')}
                variant="secondary"
                className="w-full text-left p-4 h-auto bg-gray-700 hover:bg-gray-600 text-white"
              >
                <div className="flex items-center justify-between">
                  <div>
                    <div className="font-bold">PF3 - Exit</div>
                    <div className="text-sm opacity-80">Return to main application</div>
                  </div>
                  <div className="text-2xl">←</div>
                </div>
              </Button>
            </div>
          </div>

          {/* Message area */}
          {message && (
            <div className={`mt-6 p-3 border ${
              messageType === 'success' ? 'border-green-400 text-green-400' :
              messageType === 'error' ? 'border-red-400 text-red-400' :
              'border-yellow-400 text-yellow-400'
            } bg-gray-800`}>
              {message}
            </div>
          )}

          {/* Instructions */}
          <div className="mt-8 text-gray-500 text-sm">
            <p>Instructions:</p>
            <ul className="list-disc list-inside mt-2 space-y-1">
              <li>Click on a menu option or press the corresponding number key</li>
              <li>Press PF3 or F3 to exit the administration menu</li>
              <li>Administrator privileges are required for user management functions</li>
              {userType !== 'ADMIN' && (
                <li className="text-red-400">
                  <strong>Note:</strong> You have limited access. Contact an administrator for user management tasks.
                </li>
              )}
            </ul>
          </div>
        </div>
      </div>
    </div>
  );
}