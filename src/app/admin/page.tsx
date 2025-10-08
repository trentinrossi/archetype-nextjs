'use client';

import { useState, useEffect, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui';
import userSecurityService from '@/services/userSecurityService';
import {
  ExitRequest,
  InvalidKeyRequest
} from '@/types/userSecurity';

interface AdminMenuState {
  isLoading: boolean;
  error: string;
  message: string;
  currentDateTime: string;
  selectedOption: string;
}

interface MenuOption {
  key: string;
  label: string;
  description: string;
  route: string;
  functionKey: string;
}

export default function AdminMenuPage() {
  const router = useRouter();

  const [state, setState] = useState<AdminMenuState>({
    isLoading: false,
    error: '',
    message: '',
    currentDateTime: '',
    selectedOption: ''
  });

  const menuOptions: MenuOption[] = [
    {
      key: '1',
      label: 'List Users',
      description: 'Display and manage existing users',
      route: '/users',
      functionKey: 'F1'
    },
    {
      key: '2',
      label: 'Add User',
      description: 'Create a new user account',
      route: '/users/add',
      functionKey: 'F2'
    },
    {
      key: '3',
      label: 'Update User',
      description: 'Modify existing user information',
      route: '/users/update',
      functionKey: 'F3'
    },
    {
      key: '4',
      label: 'Delete User',
      description: 'Remove user account from system',
      route: '/users/delete',
      functionKey: 'F4'
    }
  ];

  // Update current date and time
  const updateDateTime = useCallback((): void => {
    const now = new Date();
    const dateTimeString = now.toLocaleString('en-US', {
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
      hour12: false
    });
    setState(prev => ({ ...prev, currentDateTime: dateTimeString }));
  }, []);

  // Initialize page
  useEffect(() => {
    updateDateTime();
    
    // Update time every second
    const interval = setInterval(updateDateTime, 1000);
    
    setState(prev => ({ 
      ...prev, 
      message: 'Welcome to User Administration. Select an option or use function keys.' 
    }));

    return () => clearInterval(interval);
  }, [updateDateTime]);

  // Navigate to selected menu option
  const navigateToOption = useCallback((option: MenuOption): void => {
    setState(prev => ({ 
      ...prev, 
      isLoading: true, 
      error: '', 
      message: `Navigating to ${option.label}...`,
      selectedOption: option.key
    }));

    // Simulate brief loading before navigation
    setTimeout(() => {
      router.push(option.route);
    }, 500);
  }, [router]);

  // Handle menu selection by number
  const handleMenuSelection = useCallback((optionKey: string): void => {
    const option = menuOptions.find(opt => opt.key === optionKey);
    
    if (option) {
      navigateToOption(option);
    } else {
      setState(prev => ({ 
        ...prev, 
        error: `Invalid menu selection: ${optionKey}. Please select 1-4.` 
      }));
    }
  }, [menuOptions, navigateToOption]);

  // Handle exit (PF12)
  const handleExit = useCallback(async (): Promise<void> => {
    setState(prev => ({ ...prev, isLoading: true, message: 'Exiting application...' }));

    try {
      const exitRequest: ExitRequest = {};
      await userSecurityService.exit(exitRequest);
      
      // Navigate to sign-on or home page
      router.push('/');
    } catch (error) {
      console.error('Exit error:', error);
      // Still navigate even if exit service fails
      router.push('/');
    } finally {
      setState(prev => ({ ...prev, isLoading: false }));
    }
  }, [router]);

  // Handle invalid key press
  const handleInvalidKey = useCallback(async (key: string): Promise<void> => {
    try {
      const invalidKeyRequest: InvalidKeyRequest = {
        attemptedKey: key,
        timestamp: new Date().toISOString()
      };

      const response = await userSecurityService.handleInvalidKey(invalidKeyRequest);
      
      if (response.success && response.data?.message) {
        setState(prev => ({ ...prev, error: response.data!.message }));
      } else {
        setState(prev => ({ 
          ...prev, 
          error: `Invalid key pressed: ${key}. Use 1-4 to select menu options, F1-F4 for direct access, or F12 to exit.` 
        }));
      }
    } catch (error) {
      console.error('Invalid key handling error:', error);
      setState(prev => ({ 
        ...prev, 
        error: `Invalid key pressed: ${key}. Use 1-4 to select menu options, F1-F4 for direct access, or F12 to exit.` 
      }));
    }
  }, []);

  // Handle keyboard events
  const handleKeyDown = useCallback(async (event: React.KeyboardEvent): Promise<void> => {
    // Prevent default for function keys and numbers
    if (event.key.startsWith('F') || /^[1-4]$/.test(event.key)) {
      event.preventDefault();
    }

    if (state.isLoading) {
      return;
    }

    // Clear previous messages
    setState(prev => ({ ...prev, error: '', message: '' }));

    switch (event.key) {
      case '1':
        handleMenuSelection('1');
        break;
      
      case '2':
        handleMenuSelection('2');
        break;
      
      case '3':
        handleMenuSelection('3');
        break;
      
      case '4':
        handleMenuSelection('4');
        break;
      
      case 'F1':
        navigateToOption(menuOptions[0]);
        break;
      
      case 'F2':
        navigateToOption(menuOptions[1]);
        break;
      
      case 'F3':
        navigateToOption(menuOptions[2]);
        break;
      
      case 'F4':
        navigateToOption(menuOptions[3]);
        break;
      
      case 'F12':
        await handleExit();
        break;
      
      case 'F5':
      case 'F6':
      case 'F7':
      case 'F8':
      case 'F9':
      case 'F10':
      case 'F11':
      case 'Enter':
      case 'Escape':
        await handleInvalidKey(event.key);
        break;
      
      default:
        // Handle other special key combinations as invalid
        if (event.ctrlKey || event.altKey || event.metaKey) {
          await handleInvalidKey(`${event.ctrlKey ? 'Ctrl+' : ''}${event.altKey ? 'Alt+' : ''}${event.metaKey ? 'Meta+' : ''}${event.key}`);
        } else if (event.key.length === 1 && !/^[1-4]$/.test(event.key)) {
          // Single character that's not 1-4
          await handleInvalidKey(event.key);
        }
        break;
    }
  }, [state.isLoading, handleMenuSelection, navigateToOption, menuOptions, handleExit, handleInvalidKey]);

  // Handle button clicks
  const handleOptionClick = useCallback((option: MenuOption): void => {
    if (!state.isLoading) {
      navigateToOption(option);
    }
  }, [state.isLoading, navigateToOption]);

  return (
    <div 
      className="min-h-screen bg-gray-50 py-8 px-4 sm:px-6 lg:px-8"
      onKeyDown={handleKeyDown}
      tabIndex={-1}
    >
      <div className="max-w-4xl mx-auto">
        {/* Header */}
        <div className="bg-white shadow rounded-lg p-6 mb-8">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-3xl font-bold text-gray-900">Main Administration Menu</h1>
              <p className="mt-1 text-sm text-gray-600">Transaction ID: COADM01C</p>
            </div>
            <div className="text-right">
              <p className="text-sm text-gray-600">Current Date/Time:</p>
              <p className="text-lg font-mono text-gray-900">{state.currentDateTime}</p>
            </div>
          </div>
        </div>

        {/* Messages */}
        {state.error && (
          <div className="mb-6 rounded-md bg-red-50 p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <svg className="h-5 w-5 text-red-400" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-3">
                <p className="text-sm text-red-800">{state.error}</p>
              </div>
            </div>
          </div>
        )}

        {state.message && !state.error && (
          <div className="mb-6 rounded-md bg-blue-50 p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <svg className="h-5 w-5 text-blue-400" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-3">
                <p className="text-sm text-blue-800">{state.message}</p>
              </div>
            </div>
          </div>
        )}

        {/* Menu Options */}
        <div className="bg-white shadow rounded-lg p-6 mb-8">
          <h2 className="text-xl font-semibold text-gray-900 mb-6">User Management Functions</h2>
          
          <div className="grid gap-4 md:grid-cols-2">
            {menuOptions.map((option) => (
              <div
                key={option.key}
                className={`relative rounded-lg border-2 p-4 cursor-pointer transition-all duration-200 ${
                  state.selectedOption === option.key
                    ? 'border-blue-500 bg-blue-50'
                    : 'border-gray-200 hover:border-gray-300 hover:bg-gray-50'
                } ${state.isLoading ? 'opacity-50 cursor-not-allowed' : ''}`}
                onClick={() => handleOptionClick(option)}
              >
                <div className="flex items-start justify-between">
                  <div className="flex-1">
                    <div className="flex items-center space-x-3">
                      <span className="inline-flex items-center justify-center w-8 h-8 rounded-full bg-blue-100 text-blue-800 text-sm font-medium">
                        {option.key}
                      </span>
                      <div>
                        <h3 className="text-lg font-medium text-gray-900">{option.label}</h3>
                        <p className="text-sm text-gray-600">{option.description}</p>
                      </div>
                    </div>
                  </div>
                  <div className="flex-shrink-0">
                    <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-gray-100 text-gray-800">
                      {option.functionKey}
                    </span>
                  </div>
                </div>
                
                {state.isLoading && state.selectedOption === option.key && (
                  <div className="absolute inset-0 flex items-center justify-center bg-white bg-opacity-75 rounded-lg">
                    <div className="animate-spin rounded-full h-6 w-6 border-b-2 border-blue-600"></div>
                  </div>
                )}
              </div>
            ))}
          </div>
        </div>

        {/* Action Buttons */}
        <div className="flex justify-between">
          <div className="flex space-x-4">
            {menuOptions.map((option) => (
              <Button
                key={option.key}
                onClick={() => handleOptionClick(option)}
                disabled={state.isLoading}
                className="px-4 py-2"
                variant={state.selectedOption === option.key ? 'primary' : 'secondary'}
              >
                {option.key}. {option.label}
              </Button>
            ))}
          </div>

          <Button
            onClick={handleExit}
            disabled={state.isLoading}
            variant="secondary"
            className="px-6 py-2"
          >
            Exit (F12)
          </Button>
        </div>

        {/* Instructions */}
        <div className="mt-8 bg-gray-50 rounded-lg p-6">
          <h3 className="text-lg font-medium text-gray-900 mb-4">Navigation Instructions</h3>
          
          <div className="grid gap-6 md:grid-cols-2">
            <div>
              <h4 className="text-sm font-medium text-gray-900 mb-2">Menu Selection:</h4>
              <ul className="text-sm text-gray-600 space-y-1">
                <li>• Press <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">1</kbd> or <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F1</kbd> for List Users (COUSR00C)</li>
                <li>• Press <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">2</kbd> or <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F2</kbd> for Add User (COUSR01C)</li>
                <li>• Press <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">3</kbd> or <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F3</kbd> for Update User (COUSR02C)</li>
                <li>• Press <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">4</kbd> or <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F4</kbd> for Delete User (COUSR03C)</li>
              </ul>
            </div>
            
            <div>
              <h4 className="text-sm font-medium text-gray-900 mb-2">System Functions:</h4>
              <ul className="text-sm text-gray-600 space-y-1">
                <li>• Press <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F12</kbd> to exit the application</li>
                <li>• Click on menu options to navigate</li>
                <li>• Use keyboard shortcuts for quick access</li>
                <li>• Current date and time displayed in header</li>
              </ul>
            </div>
          </div>

          <div className="mt-4 p-3 bg-yellow-50 rounded-md">
            <p className="text-sm text-yellow-800">
              <strong>Note:</strong> This is the central administration menu for user management functions. 
              All user security operations are accessed through this menu.
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}